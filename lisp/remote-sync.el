;;; remote-sync.el --- Seamless remote project sync with native git -*- lexical-binding: t -*-

;; Author: Claude
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (projectile "2.0") (magit "3.0"))
;; Keywords: tools, remote, sync

;;; Commentary:
;; Provides seamless remote project editing with:
;; - Mutagen-based file sync (local cache of remote projects)
;; - Native Magit integration (git operations happen on remote)
;; - LSP support via TRAMP
;; - VTerm integration for remote shells

;;; Code:

(require 'projectile)
(require 'tramp)
(require 'magit nil t)
(require 'vterm nil t)

(defgroup remote-sync nil
  "Sync remote projects locally with Mutagen."
  :group 'tools
  :prefix "remote-sync-")

(defcustom remote-sync-cache-directory "~/.remote-sync"
  "Local cache directory for synced remote projects."
  :type 'directory
  :group 'remote-sync)

(defcustom remote-sync-hosts '()
  "List of SSH hosts. If empty, auto-discovered from SSH config."
  :type '(repeat string)
  :group 'remote-sync)

(defcustom remote-sync-ignores
  '("node_modules" "__pycache__" ".venv" "venv"
    "target" "build" "dist" ".cache" "*.pyc" ".DS_Store"
    ".mypy_cache" ".pytest_cache" ".tox" "*.egg-info")
  "Patterns to ignore in sync."
  :type '(repeat string)
  :group 'remote-sync)

(defcustom remote-sync-ssh-config-file "~/.ssh/config"
  "Path to SSH config file."
  :type 'file
  :group 'remote-sync)

(defcustom remote-sync-project-expiry-days 7
  "Number of days after which unused projects are considered expired.
Set to nil to disable automatic expiration."
  :type '(choice (integer :tag "Days")
                 (const :tag "Never expire" nil))
  :group 'remote-sync)

(defvar remote-sync--projects (make-hash-table :test 'equal)
  "Map local cache paths to (remote-tramp-path . last-accessed-time).")

(defun remote-sync--hash-to-alist (table)
  "Convert hash TABLE to an alist."
  (let (alist)
    (maphash (lambda (k v) (push (cons k v) alist)) table)
    alist))

;;; ============================================================
;;; Project Registry Helpers
;;; ============================================================

(defun remote-sync--get-remote-path (local-path)
  "Get remote TRAMP path for LOCAL-PATH from registry."
  (when-let ((entry (gethash local-path remote-sync--projects)))
    (if (consp entry) (car entry) entry)))  ; Handle both old and new format

(defun remote-sync--get-last-accessed (local-path)
  "Get last accessed time for LOCAL-PATH from registry."
  (when-let ((entry (gethash local-path remote-sync--projects)))
    (if (consp entry) (cdr entry) nil)))

(defun remote-sync--set-project (local-path remote-path)
  "Register LOCAL-PATH with REMOTE-PATH and update access time."
  (puthash local-path (cons remote-path (current-time)) remote-sync--projects))

(defun remote-sync--touch-project (local-path)
  "Update last accessed time for LOCAL-PATH."
  (when-let ((remote-path (remote-sync--get-remote-path local-path)))
    (puthash local-path (cons remote-path (current-time)) remote-sync--projects)
    (remote-sync--save-registry)))

;;; ============================================================
;;; SSH Config & Host Management
;;; ============================================================

(defun remote-sync--parse-ssh-hosts ()
  "Parse SSH config file and return list of hosts."
  (let ((config-file (expand-file-name remote-sync-ssh-config-file))
        hosts)
    (when (file-exists-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (while (re-search-forward "^Host[ \t]+\\([^ \t\n*?]+\\)" nil t)
          (let ((host (match-string 1)))
            (unless (or (string= host "*")
                        (string-prefix-p "#" (string-trim host)))
              (push host hosts))))))
    (nreverse hosts)))

(defun remote-sync--get-hosts ()
  "Get list of available SSH hosts."
  (or remote-sync-hosts (remote-sync--parse-ssh-hosts)))

(defun remote-sync--select-host (&optional prompt)
  "Interactively select an SSH host with PROMPT."
  (let ((hosts (remote-sync--get-hosts)))
    (if (null hosts)
        (progn
          (message "No hosts found. Add hosts to SSH config first.")
          (when (yes-or-no-p "Open SSH config now? ")
            (remote-sync-edit-ssh-config))
          nil)
      (completing-read (or prompt "Select host: ") hosts nil t))))

;;;###autoload
(defun remote-sync-edit-ssh-config ()
  "Open SSH config file for editing."
  (interactive)
  (let ((config-file (expand-file-name remote-sync-ssh-config-file)))
    (unless (file-exists-p config-file)
      (make-directory (file-name-directory config-file) t)
      (write-region "" nil config-file))
    (find-file config-file)
    (message "Example: Host myserver, HostName 1.2.3.4, User ubuntu, IdentityFile ~/.ssh/key.pem")))

;;; ============================================================
;;; Path Utilities
;;; ============================================================

(defun remote-sync--get-remote-user (host)
  "Get username for HOST from SSH config."
  (let ((config-file (expand-file-name remote-sync-ssh-config-file))
        (user nil))
    (when (file-exists-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (when (re-search-forward
               (format "^Host\\s-+%s\\s-*$" (regexp-quote host)) nil t)
          (let ((section-end (save-excursion
                               (if (re-search-forward "^Host\\s-" nil t)
                                   (match-beginning 0)
                                 (point-max)))))
            (when (re-search-forward "^\\s-*User\\s-+\\(\\S-+\\)" section-end t)
              (setq user (match-string 1)))))))
    (or user "ubuntu")))

(defun remote-sync--cache-path (host remote-path)
  "Get local cache path for HOST:REMOTE-PATH."
  (let* ((user (remote-sync--get-remote-user host))
         (normalized-path
          (cond
           ((string-prefix-p "~/" remote-path)
            (concat "/home/" user (substring remote-path 1)))
           ((string= "~" remote-path)
            (concat "/home/" user))
           ((string-prefix-p "/" remote-path)
            remote-path)
           (t (concat "/home/" user "/" remote-path)))))
    (expand-file-name (concat host normalized-path) remote-sync-cache-directory)))

(defun remote-sync--tramp-path (host remote-path)
  "Get TRAMP path for HOST:REMOTE-PATH.
Expands tilde to full home directory path for TRAMP compatibility."
  (let ((expanded-path
         (if (string-prefix-p "~" remote-path)
             ;; Expand tilde to /home/user
             (concat "/home/" (remote-sync--get-remote-user host)
                     (substring remote-path 1))
           remote-path)))
    (format "/ssh:%s:%s" host expanded-path)))

(defun remote-sync--local-to-remote (local-path)
  "Get remote TRAMP path for LOCAL-PATH if it's a synced project."
  ;; Quick check: if not under cache directory, skip expensive lookup
  (when (and local-path
             (string-prefix-p (expand-file-name remote-sync-cache-directory)
                              (expand-file-name local-path)))
    (let ((expanded (expand-file-name local-path)))
      (cl-loop for local being the hash-keys of remote-sync--projects
               for remote = (remote-sync--get-remote-path local)
               when (and remote (string-prefix-p local expanded))
               return (concat remote (substring expanded (length local)))))))

(defun remote-sync--project-info (local-path)
  "Get (host . remote-path) for LOCAL-PATH."
  (when-let ((tramp-path (remote-sync--local-to-remote local-path)))
    (with-parsed-tramp-file-name tramp-path parsed
      (cons parsed-host parsed-localname))))

(defun remote-sync--current-project-host ()
  "Get the host for current project if it's a synced project."
  (when-let* ((root (ignore-errors (projectile-project-root)))
              (info (remote-sync--project-info root)))
    (car info)))

;;; ============================================================
;;; Remote Directory Browsing
;;; ============================================================

(defun remote-sync--is-project-root-p (host path)
  "Check if PATH on HOST looks like a project root."
  (let* ((tramp-path (remote-sync--tramp-path host path))
         (project-markers '(".git" "package.json" "Cargo.toml" "pyproject.toml"
                            "setup.py" "go.mod" "Makefile" ".projectile"
                            "requirements.txt" "pom.xml" "build.gradle")))
    (condition-case nil
        (cl-some (lambda (marker)
                   (file-exists-p (expand-file-name marker tramp-path)))
                 project-markers)
      (error nil))))

(defun remote-sync--list-directory (host path)
  "List directories in PATH on HOST."
  (let ((tramp-path (remote-sync--tramp-path host path)))
    (condition-case nil
        (cl-remove-if-not
         (lambda (f)
           (and (not (string-prefix-p "." f))
                (file-directory-p (expand-file-name f tramp-path))))
         (directory-files tramp-path nil "^[^.]"))
      (error nil))))

(defun remote-sync--join-remote-path (base name)
  "Join BASE and NAME for remote paths (don't use expand-file-name which uses local fs)."
  (let ((base-clean (if (string-suffix-p "/" base)
                        (substring base 0 -1)
                      base)))
    (concat base-clean "/" name)))

(defun remote-sync--parent-remote-path (path)
  "Get parent directory of remote PATH."
  (let ((clean-path (if (string-suffix-p "/" path)
                        (substring path 0 -1)
                      path)))
    (if (or (string= clean-path "~")
            (string= clean-path "/")
            (not (string-match-p "/" clean-path)))
        nil
      (file-name-directory clean-path))))

(defun remote-sync--browse-remote (host &optional current-path)
  "Browse directories on HOST starting from CURRENT-PATH.
Returns selected directory path or nil if cancelled."
  (let* ((path (or current-path "~"))
         (_ (message "Browsing %s on %s..." path host))
         (dirs (remote-sync--list-directory host path))
         (is-project (remote-sync--is-project-root-p host path))
         (choices (append
                   (when is-project '(">> SELECT THIS DIRECTORY <<"))
                   '(".. (up)")
                   (mapcar (lambda (d) (concat d "/")) dirs))))
    ;; Always show choices if we have any dirs or it's a project
    (if (and (null dirs) (not is-project))
        ;; Empty non-project directory
        (if (yes-or-no-p (format "%s appears empty and is not a project. Go up? " path))
            (let ((parent (remote-sync--parent-remote-path path)))
              (if parent
                  (remote-sync--browse-remote host parent)
                nil))
          nil)
      (let* ((prompt (if is-project
                         (format "[%s] %s (PROJECT) > " host path)
                       (format "[%s] %s > " host path)))
             (selection (completing-read prompt choices nil t)))
        (cond
         ((or (null selection) (string-empty-p selection))
          nil)
         ((string= selection ">> SELECT THIS DIRECTORY <<")
          path)
         ((string= selection ".. (up)")
          (let ((parent (remote-sync--parent-remote-path path)))
            (if parent
                (remote-sync--browse-remote host parent)
              (progn (message "Already at root") (remote-sync--browse-remote host path)))))
         (t
          (let ((new-path (remote-sync--join-remote-path path (string-remove-suffix "/" selection))))
            ;; Check if it's a project and select directly
            (message "Checking if %s is a project..." new-path)
            (if (remote-sync--is-project-root-p host new-path)
                (progn
                  (message "Found project: %s" new-path)
                  new-path)
              ;; Otherwise, keep browsing
              (remote-sync--browse-remote host new-path)))))))))

(defun remote-sync--find-git-projects (host base-path &optional max-depth)
  "Find git projects on HOST under BASE-PATH up to MAX-DEPTH."
  (message "Searching for git repos on %s (this may take a moment)..." host)
  (let* ((depth (or max-depth 3))
         (tramp-path (remote-sync--tramp-path host base-path))
         (cmd (format "find %s -maxdepth %d -type d -name .git 2>/dev/null | head -50"
                      (shell-quote-argument base-path)
                      depth))
         (default-directory tramp-path)
         (output (shell-command-to-string cmd)))
    (when (not (string-empty-p output))
      (mapcar (lambda (git-dir)
                (directory-file-name (file-name-directory git-dir)))
              (split-string (string-trim output) "\n" t)))))

(defun remote-sync--project-already-cloned-p (host remote-path)
  "Check if project at REMOTE-PATH from HOST is already cloned locally."
  (cl-loop for local being the hash-keys of remote-sync--projects
           for remote = (remote-sync--get-remote-path local)
           when (and remote (string= remote (remote-sync--tramp-path host remote-path)))
           return local))

;;;###autoload
(defun remote-sync-open ()
  "Open a remote project. Syncs automatically if not already cloned.
Browse remote filesystem to select a project."
  (interactive)
  (when-let ((host (remote-sync--select-host "Open from host: ")))
    (let ((method (completing-read
                   "How to find project? "
                   '("Browse from home directory"
                     "Search for git repos"
                     "Enter path manually")
                   nil t)))
      (when-let ((remote-path
                  (pcase method
                    ("Browse from home directory"
                     (message "Connecting to %s..." host)
                     (remote-sync--browse-remote host "~"))
                    ("Search for git repos"
                     (let ((base (read-string "Search under (default ~): " nil nil "~")))
                       (if-let ((projects (remote-sync--find-git-projects host base)))
                           (completing-read "Select project: " projects nil t)
                         (message "No git repos found under %s" base)
                         nil)))
                    ("Enter path manually"
                     (read-string "Remote path: " "~/")))))
        (if-let ((existing (remote-sync--project-already-cloned-p host remote-path)))
            (progn
              (message "Opening synced project: %s" existing)
              (remote-sync--touch-project existing)
              (projectile-switch-project-by-name existing))
          (remote-sync--do-clone host remote-path))))))

;; Keep old name as alias for compatibility
(defalias 'remote-sync-clone 'remote-sync-open)

(defun remote-sync--expand-remote-path (host path)
  "Expand PATH for remote HOST, converting ~ to full home directory."
  (if (string-prefix-p "~" path)
      (concat "/home/" (remote-sync--get-remote-user host) (substring path 1))
    path))

(defun remote-sync--do-clone (host remote-path)
  "Clone project from HOST at REMOTE-PATH."
  ;; Ensure Mutagen daemon is running
  (remote-sync--ensure-mutagen-daemon)
  ;; Expand tilde to full path for Mutagen
  (let* ((expanded-remote-path (remote-sync--expand-remote-path host remote-path))
         (local-path (remote-sync--cache-path host remote-path))
         (project-name (file-name-nondirectory (directory-file-name remote-path)))
         ;; Mutagen sync names: lowercase alphanumeric and hyphens only
         (sync-name (downcase
                     (replace-regexp-in-string
                      "[^a-zA-Z0-9]+" "-"
                      (format "%s-%s" host project-name)))))

    ;; Create cache directory
    (make-directory local-path t)

    ;; Start Mutagen sync
    (message "Starting Mutagen sync for %s..." project-name)
    (message "Remote: %s:%s" host expanded-remote-path)
    (message "Local: %s" local-path)
    (let* ((ignore-args (mapconcat (lambda (p) (format "--ignore=%s" p))
                                   remote-sync-ignores " "))
           (cmd (format "mutagen sync create %s:%s %s --name=%s %s 2>&1"
                        host expanded-remote-path local-path sync-name ignore-args))
           (_ (message "Running: %s" cmd))
           (result (shell-command-to-string cmd)))
      (when (string-match-p "Error\\|error\\|failed\\|unable" result)
        ;; Show error in a buffer so user can read it
        (let ((buf (get-buffer-create "*mutagen-error*")))
          (with-current-buffer buf
            (erase-buffer)
            (insert "Mutagen sync failed\n")
            (insert "===================\n\n")
            (insert "Command:\n")
            (insert cmd)
            (insert "\n\nOutput:\n")
            (insert result))
          (pop-to-buffer buf))
        (user-error "Failed to create mutagen sync - see *mutagen-error* buffer")))

    ;; Register the project with timestamp
    (remote-sync--set-project local-path (remote-sync--tramp-path host remote-path))
    (remote-sync--save-registry)

    ;; Wait for initial sync
    (message "Waiting for initial sync (this may take a moment)...")
    (shell-command (format "mutagen sync flush %s 2>/dev/null" sync-name))

    ;; Open project
    (projectile-switch-project-by-name local-path)
    (message "Project '%s' ready! Magit uses remote git automatically." project-name)))

;;; ============================================================
;;; VTerm Integration
;;; ============================================================

(defun remote-sync--vterm-remote (host &optional directory)
  "Open vterm connected to HOST in DIRECTORY."
  (unless (require 'vterm nil t)
    (user-error "vterm is not installed. Add (vterm) to packages.el"))
  (let* ((dir (or directory "~"))
         (dir-name (if (string= dir "~")
                       "~"
                     (file-name-nondirectory (directory-file-name dir))))
         (buffer-name (format "*vterm:%s:%s*" host dir-name))
         (buffer (get-buffer buffer-name)))
    (if (and buffer (buffer-live-p buffer))
        (switch-to-buffer buffer)
      ;; Start vterm with SSH as the shell command directly
      (let ((vterm-shell (format "ssh -t %s 'cd %s && exec $SHELL -l'" host dir)))
        (vterm buffer-name)))))

;;;###autoload
(defun remote-sync-vterm ()
  "Open vterm on remote machine.
If in a synced project, opens terminal on that host in project dir.
Otherwise, prompts to select a host."
  (interactive)
  (if-let* ((root (ignore-errors (projectile-project-root)))
            (info (remote-sync--project-info root)))
      (remote-sync--vterm-remote (car info) (cdr info))
    (when-let ((host (remote-sync--select-host "Terminal on: ")))
      (remote-sync--vterm-remote host))))

;;;###autoload
(defun remote-sync-vterm-select ()
  "Open vterm on a selected host (always prompts for host selection)."
  (interactive)
  (when-let ((host (remote-sync--select-host "Terminal on: ")))
    (remote-sync--vterm-remote host)))

;;;###autoload
(defun remote-sync-vterm-project ()
  "Open vterm in current synced project's remote directory."
  (interactive)
  (if-let* ((root (projectile-project-root))
            (info (remote-sync--project-info root)))
      (remote-sync--vterm-remote (car info) (cdr info))
    (user-error "Not in a synced remote project")))

;;; ============================================================
;;; Magit Integration
;;; ============================================================

(defun remote-sync--with-remote-dir (orig-fun &rest args)
  "Run ORIG-FUN with `default-directory' set to remote if in synced project."
  (if-let ((remote-path (remote-sync--local-to-remote default-directory)))
      (let ((default-directory remote-path))
        (apply orig-fun args))
    (apply orig-fun args)))

(defun remote-sync-setup-magit ()
  "Set up Magit to transparently use remote git for synced projects."
  ;; Core functions that determine git directory location
  (advice-add 'magit-toplevel :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-git-dir :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-rev-parse-safe :around #'remote-sync--with-remote-dir)

  ;; Git process execution functions
  (advice-add 'magit-process-file :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-start-process :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-call-process :around #'remote-sync--with-remote-dir)

  ;; Git command runners
  (advice-add 'magit-run-git :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-run-git-with-input :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-run-git-async :around #'remote-sync--with-remote-dir)

  ;; Git output functions
  (advice-add 'magit-git-wash :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-git-insert :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-git-string :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-git-lines :around #'remote-sync--with-remote-dir)
  (advice-add 'magit-git-items :around #'remote-sync--with-remote-dir))

(with-eval-after-load 'magit
  (remote-sync-setup-magit))

;;; ============================================================
;;; Buffer Setup
;;; ============================================================

(defun remote-sync--setup-buffer ()
  "Set up buffer for synced project (disable vc-mode, set markers)."
  (when-let* ((file (buffer-file-name))
              (root (ignore-errors (projectile-project-root)))
              (remote-root (remote-sync--local-to-remote root)))
    ;; Mark buffer as being in a synced project
    (setq-local remote-sync-remote-root remote-root)
    ;; Disable vc-mode - we use magit directly via TRAMP
    (setq-local vc-handled-backends nil)))

(add-hook 'find-file-hook #'remote-sync--setup-buffer)

;;; ============================================================
;;; LSP Integration
;;; ============================================================

(defun remote-sync--lsp-root-override (orig-fun &rest args)
  "Use remote path as LSP workspace root for synced projects."
  (if-let ((remote-path (remote-sync--local-to-remote default-directory)))
      remote-path
    (apply orig-fun args)))

(with-eval-after-load 'lsp-mode
  (setq lsp-enable-file-watchers nil)
  (advice-add 'lsp-workspace-root :around #'remote-sync--lsp-root-override))

;;; ============================================================
;;; Registry Persistence
;;; ============================================================

(defun remote-sync--registry-file ()
  "Get path to the project registry file."
  (expand-file-name "registry.el" remote-sync-cache-directory))

(defun remote-sync--save-registry ()
  "Save project registry to disk."
  (make-directory remote-sync-cache-directory t)
  (with-temp-file (remote-sync--registry-file)
    (insert ";; Remote-sync project registry -*- lisp-data -*-\n")
    (insert ";; Do not edit manually\n\n")
    (prin1 (remote-sync--hash-to-alist remote-sync--projects) (current-buffer))))

(defun remote-sync--load-registry ()
  "Load project registry from disk."
  (when (file-exists-p (remote-sync--registry-file))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents (remote-sync--registry-file))
          (goto-char (point-min))
          ;; Skip comments
          (while (looking-at "^;")
            (forward-line 1))
          (let ((alist (read (current-buffer))))
            (clrhash remote-sync--projects)
            (dolist (pair alist)
              (let ((local-path (car pair))
                    (value (cdr pair)))
                ;; Handle both old format (string) and new format (cons)
                (if (consp value)
                    (puthash local-path value remote-sync--projects)
                  ;; Old format: just remote-path, add current time
                  (puthash local-path (cons value (current-time)) remote-sync--projects))))))
      (error
       (message "Warning: Could not load remote-sync registry: %s"
                (error-message-string err))))))

(defun remote-sync--project-expired-p (local-path)
  "Check if project at LOCAL-PATH has expired."
  (when remote-sync-project-expiry-days
    (when-let ((last-accessed (remote-sync--get-last-accessed local-path)))
      (> (float-time (time-subtract (current-time) last-accessed))
         (* remote-sync-project-expiry-days 24 60 60)))))

(defun remote-sync--cleanup-expired-projects ()
  "Remove expired projects from registry and optionally delete local files."
  (let ((expired-projects '()))
    ;; Find expired projects
    (maphash (lambda (local-path _)
               (when (remote-sync--project-expired-p local-path)
                 (push local-path expired-projects)))
             remote-sync--projects)
    ;; Remove them
    (when expired-projects
      (dolist (local-path expired-projects)
        (let* ((project-name (file-name-nondirectory (directory-file-name local-path)))
               (sync-name (downcase
                           (replace-regexp-in-string
                            "[^a-zA-Z0-9]+" "-"
                            (file-name-nondirectory (directory-file-name local-path))))))
          ;; Terminate mutagen sync (ignore errors - host may be gone)
          (ignore-errors
            (shell-command (format "mutagen sync terminate %s 2>/dev/null" sync-name)))
          ;; Remove local cache if it exists
          (when (file-exists-p local-path)
            (delete-directory local-path t))
          ;; Remove from registry
          (remhash local-path remote-sync--projects)
          ;; Also remove from venvs registry
          (when (boundp 'remote-sync--project-venvs)
            (remhash local-path remote-sync--project-venvs))
          ;; Remove from projectile known projects
          (when (fboundp 'projectile-remove-known-project)
            (projectile-remove-known-project local-path))
          (message "Cleaned up expired project: %s" project-name)))
      (remote-sync--save-registry)
      (when (boundp 'remote-sync--project-venvs)
        (remote-sync--save-venv-registry))
      (message "Cleaned up %d expired project(s)" (length expired-projects)))))

;;;###autoload
(defun remote-sync-cleanup ()
  "Manually clean up expired projects."
  (interactive)
  (remote-sync--cleanup-expired-projects)
  (message "Cleanup complete"))

;; Load registry on startup and clean up expired projects
(remote-sync--load-registry)
(when remote-sync-project-expiry-days
  (remote-sync--cleanup-expired-projects))

;;; ============================================================
;;; Mutagen Daemon Management
;;; ============================================================

(defun remote-sync--mutagen-daemon-running-p ()
  "Check if Mutagen daemon is running."
  (= 0 (call-process "mutagen" nil nil nil "daemon" "status")))

(defun remote-sync--ensure-mutagen-daemon ()
  "Ensure Mutagen daemon is running, starting it if necessary."
  (unless (remote-sync--mutagen-daemon-running-p)
    (message "Starting Mutagen daemon...")
    (let ((result (call-process "mutagen" nil nil nil "daemon" "start")))
      (if (= 0 result)
          (progn
            (message "Mutagen daemon started")
            ;; Give it a moment to initialize
            (sleep-for 0.5))
        (user-error "Failed to start Mutagen daemon. Is mutagen installed?")))))

;;;###autoload
(defun remote-sync-stop-daemon ()
  "Stop the Mutagen daemon."
  (interactive)
  (if (remote-sync--mutagen-daemon-running-p)
      (progn
        (shell-command "mutagen daemon stop")
        (message "Mutagen daemon stopped"))
    (message "Mutagen daemon is not running")))

;;;###autoload
(defun remote-sync-daemon-status ()
  "Check and display Mutagen daemon status."
  (interactive)
  (if (remote-sync--mutagen-daemon-running-p)
      (message "Mutagen daemon is running")
    (message "Mutagen daemon is not running")))

;;; ============================================================
;;; Utility Commands
;;; ============================================================

;;;###autoload
(defun remote-sync-status ()
  "Show Mutagen sync status in a buffer."
  (interactive)
  (remote-sync--ensure-mutagen-daemon)
  (let ((buf (get-buffer-create "*mutagen-status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (shell-command-to-string "mutagen sync list 2>&1")))
      (special-mode)
      (local-set-key "g" (lambda () (interactive) (remote-sync-status)))
      (local-set-key "q" 'quit-window))
    (display-buffer buf)))

;;;###autoload
(defun remote-sync-run (cmd)
  "Run CMD on remote in current synced project."
  (interactive "sCommand: ")
  (if-let ((remote-root (remote-sync--local-to-remote (projectile-project-root))))
      (let ((default-directory remote-root))
        (compile cmd))
    (user-error "Not in a synced remote project")))

;;;###autoload
(defun remote-sync-remove-project ()
  "Stop sync and remove a project from the registry."
  (interactive)
  (if (hash-table-empty-p remote-sync--projects)
      (message "No synced projects to remove")
    (let* ((projects (hash-table-keys remote-sync--projects))
           (local-path (completing-read "Remove project: " projects nil t)))
      (when (yes-or-no-p (format "Remove '%s'? " (file-name-nondirectory
                                                  (directory-file-name local-path))))
        (let* ((info (remote-sync--project-info local-path))
               (sync-name (format "%s_%s"
                                  (car info)
                                  (replace-regexp-in-string
                                   "[/~]" "_"
                                   (directory-file-name (cdr info))))))
          ;; Terminate mutagen sync
          (shell-command (format "mutagen sync terminate %s 2>/dev/null" sync-name))
          ;; Remove from registry
          (remhash local-path remote-sync--projects)
          (remote-sync--save-registry)
          ;; Remove from projectile known projects
          (when (fboundp 'projectile-remove-known-project)
            (projectile-remove-known-project local-path))
          ;; Remove from venvs registry
          (when (boundp 'remote-sync--project-venvs)
            (remhash local-path remote-sync--project-venvs)
            (remote-sync--save-venv-registry))
          ;; Optionally delete local cache
          (when (and (file-exists-p local-path)
                     (yes-or-no-p "Delete local cached files? "))
            (delete-directory local-path t))
          (message "Removed project"))))))

;;;###autoload
(defun remote-sync-list-projects ()
  "List all synced projects."
  (interactive)
  (if (hash-table-empty-p remote-sync--projects)
      (message "No synced projects. Use SPC R c to clone one.")
    (let ((buf (get-buffer-create "*remote-sync-projects*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Synced Remote Projects\n")
          (insert "======================\n\n")
          (maphash (lambda (local remote)
                     (insert (format "Local:  %s\n" local))
                     (insert (format "Remote: %s\n" remote))
                     (insert "\n"))
                   remote-sync--projects))
        (special-mode)
        (local-set-key "q" 'quit-window))
      (display-buffer buf))))

;;;###autoload
(defun remote-sync-open-project ()
  "Open an already-synced project."
  (interactive)
  (if (hash-table-empty-p remote-sync--projects)
      (when (yes-or-no-p "No synced projects. Open one from remote? ")
        (remote-sync-open))
    (let* ((projects (hash-table-keys remote-sync--projects))
           (display-projects (mapcar (lambda (p)
                                       (cons (file-name-nondirectory
                                              (directory-file-name p))
                                             p))
                                     projects))
           (selection (completing-read "Open project: "
                                       (mapcar #'car display-projects)
                                       nil t))
           (local-path (cdr (assoc selection display-projects))))
      ;; Update last accessed time
      (remote-sync--touch-project local-path)
      (projectile-switch-project-by-name local-path))))

;;; ============================================================
;;; Remote Python Virtualenv Management
;;; ============================================================

(defvar remote-sync--project-venvs (make-hash-table :test 'equal)
  "Map project local paths to their remote virtualenv paths.")

(defun remote-sync--venv-registry-file ()
  "Path to virtualenv registry file."
  (expand-file-name "venvs.el" remote-sync-cache-directory))

(defun remote-sync--save-venv-registry ()
  "Save virtualenv registry to disk."
  (with-temp-file (remote-sync--venv-registry-file)
    (insert ";; Remote-sync virtualenv registry -*- lisp-data -*-\n\n")
    (prin1 (remote-sync--hash-to-alist remote-sync--project-venvs) (current-buffer))))

(defun remote-sync--load-venv-registry ()
  "Load virtualenv registry from disk."
  (when (file-exists-p (remote-sync--venv-registry-file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents (remote-sync--venv-registry-file))
          (goto-char (point-min))
          (while (looking-at "^;") (forward-line 1))
          (let ((alist (read (current-buffer))))
            (clrhash remote-sync--project-venvs)
            (dolist (pair alist)
              (puthash (car pair) (cdr pair) remote-sync--project-venvs))))
      (error nil))))

(remote-sync--load-venv-registry)

(defun remote-sync--remote-file-exists-p (tramp-path)
  "Check if TRAMP-PATH exists on remote."
  (file-exists-p tramp-path))

(defun remote-sync--remote-directory-p (tramp-path)
  "Check if TRAMP-PATH is a directory on remote."
  (file-directory-p tramp-path))

(defun remote-sync--remote-shell-command (host command)
  "Run COMMAND on HOST and return output string."
  (let ((default-directory (remote-sync--tramp-path host "~")))
    (string-trim (shell-command-to-string command))))

(defun remote-sync--detect-remote-venv (host project-remote-path)
  "Detect virtualenv for project at PROJECT-REMOTE-PATH on HOST.
Returns the venv path if found, nil otherwise."
  (let* ((project-name (file-name-nondirectory (directory-file-name project-remote-path)))
         (tramp-root (remote-sync--tramp-path host project-remote-path))
         (user (remote-sync--get-remote-user host))
         (home-dir (format "/home/%s" user))
         (venv-path nil))

    (cond
     ;; Check for poetry.lock and get poetry env
     ((remote-sync--remote-file-exists-p (expand-file-name "poetry.lock" tramp-root))
      (let ((poetry-env (remote-sync--remote-shell-command
                         host
                         (format "cd %s && poetry env info -p 2>/dev/null" project-remote-path))))
        (when (and poetry-env
                   (not (string-empty-p poetry-env))
                   (not (string-match-p "could not find\\|no virtual environment" (downcase poetry-env)))
                   (remote-sync--remote-directory-p (remote-sync--tramp-path host poetry-env)))
          (setq venv-path poetry-env)
          (message "[remote-venv] Detected poetry env: %s" venv-path))))

     ;; Check for .python-version file (pyenv)
     ((remote-sync--remote-file-exists-p (expand-file-name ".python-version" tramp-root))
      (let* ((version (remote-sync--remote-shell-command
                       host
                       (format "cat %s/.python-version" project-remote-path)))
             (pyenv-path (format "%s/.pyenv/versions/%s" home-dir version)))
        (when (remote-sync--remote-directory-p (remote-sync--tramp-path host pyenv-path))
          (setq venv-path pyenv-path)
          (message "[remote-venv] Detected from .python-version: %s" venv-path))))

     ;; Check for .venv in project
     ((remote-sync--remote-directory-p (expand-file-name ".venv" tramp-root))
      (setq venv-path (format "%s/.venv" project-remote-path))
      (message "[remote-venv] Detected .venv: %s" venv-path))

     ;; Check for venv in project
     ((remote-sync--remote-directory-p (expand-file-name "venv" tramp-root))
      (setq venv-path (format "%s/venv" project-remote-path))
      (message "[remote-venv] Detected venv: %s" venv-path))

     ;; Check pyenv versions for project name match
     (t
      (let ((pyenv-path (format "%s/.pyenv/versions/%s" home-dir project-name)))
        (when (remote-sync--remote-directory-p (remote-sync--tramp-path host pyenv-path))
          (setq venv-path pyenv-path)
          (message "[remote-venv] Detected pyenv version: %s" venv-path)))))

    venv-path))

(defun remote-sync--list-remote-pyenv-versions (host)
  "List available pyenv versions on HOST."
  (let* ((user (remote-sync--get-remote-user host))
         (output (remote-sync--remote-shell-command
                  host
                  (format "ls -1 /home/%s/.pyenv/versions 2>/dev/null" user))))
    (when (and output (not (string-empty-p output)))
      (split-string output "\n" t))))

(defun remote-sync--list-remote-poetry-venvs (host)
  "List available poetry virtualenvs on HOST."
  (let* ((user (remote-sync--get-remote-user host))
         (output (remote-sync--remote-shell-command
                  host
                  (format "ls -1 /home/%s/.cache/pypoetry/virtualenvs 2>/dev/null" user))))
    (when (and output (not (string-empty-p output)))
      (mapcar (lambda (name)
                (format "/home/%s/.cache/pypoetry/virtualenvs/%s" user name))
              (split-string output "\n" t)))))

;;;###autoload
(defun remote-sync-select-venv ()
  "Interactively select a Python virtualenv for the current remote project."
  (interactive)
  (unless (projectile-project-p)
    (user-error "Not in a project"))
  (let* ((local-root (projectile-project-root))
         (remote-tramp (remote-sync--local-to-remote local-root)))
    (unless remote-tramp
      (user-error "Not in a remote-sync project"))
    (with-parsed-tramp-file-name remote-tramp parsed
      (let* ((host parsed-host)
             (remote-path parsed-localname)
             (user (remote-sync--get-remote-user host))
             (home-dir (format "/home/%s" user))
             ;; Gather available venvs
             (pyenv-versions (remote-sync--list-remote-pyenv-versions host))
             (poetry-venvs (remote-sync--list-remote-poetry-venvs host))
             ;; Build choices
             (choices (append
                       '(">> Auto-detect <<" ">> Enter path manually <<")
                       (when pyenv-versions
                         (cons "--- pyenv versions ---"
                               (mapcar (lambda (v) (format "%s/.pyenv/versions/%s" home-dir v))
                                       pyenv-versions)))
                       (when poetry-venvs
                         (cons "--- poetry venvs ---" poetry-venvs))))
             (selection (completing-read
                         (format "Select venv for %s: " (projectile-project-name))
                         choices nil nil)))
        (cond
         ((string= selection ">> Auto-detect <<")
          (if-let ((detected (remote-sync--detect-remote-venv host remote-path)))
              (remote-sync--set-project-venv local-root detected host)
            (message "[remote-venv] No virtualenv auto-detected")))

         ((string= selection ">> Enter path manually <<")
          (let ((manual-path (read-string "Remote venv path: " (format "%s/.pyenv/versions/" home-dir))))
            (if (remote-sync--remote-directory-p (remote-sync--tramp-path host manual-path))
                (remote-sync--set-project-venv local-root manual-path host)
              (user-error "Path does not exist on remote: %s" manual-path))))

         ((string-prefix-p "---" selection)
          (message "Please select a virtualenv, not a header"))

         (t
          (remote-sync--set-project-venv local-root selection host)))))))

(defun remote-sync--update-modeline-venv (venv-path host)
  "Update modeline to show remote VENV-PATH on HOST."
  (when (boundp 'pyvenv-virtual-env-name)
    (let ((venv-name (file-name-nondirectory (directory-file-name venv-path))))
      ;; Set buffer-local modeline indicator
      (setq-local pyvenv-virtual-env-name (format "[%s] %s" host venv-name))
      ;; Also set the full path variable for consistency
      (setq-local pyvenv-virtual-env (format "/ssh:%s:%s" host venv-path)))))

(defun remote-sync--set-project-venv (local-root venv-path host)
  "Set VENV-PATH as the virtualenv for project at LOCAL-ROOT on HOST."
  ;; Store in registry
  (puthash local-root venv-path remote-sync--project-venvs)
  (remote-sync--save-venv-registry)

  ;; Configure lsp-pyright for this project
  (let ((python-path (format "%s/bin/python" venv-path)))
    ;; Set buffer-local and project-local variables
    (setq-local lsp-pyright-venv-path (file-name-directory venv-path))
    (setq-local lsp-pyright-venv-directory (file-name-nondirectory venv-path))
    (setq-local lsp-pyright-python-executable-cmd python-path)

    ;; Update modeline indicator
    (remote-sync--update-modeline-venv venv-path host)

    ;; Write .dir-locals.el to the local cache so it persists
    (let ((dir-locals-file (expand-file-name ".dir-locals.el" local-root)))
      (with-temp-file dir-locals-file
        (insert ";;; Directory Local Variables -*- no-byte-compile: t -*-\n")
        (insert ";;; Auto-generated by remote-sync for remote Python virtualenv\n\n")
        (prin1 `((python-mode . ((lsp-pyright-venv-path . ,(file-name-directory venv-path))
                                 (lsp-pyright-venv-directory . ,(file-name-nondirectory venv-path))
                                 (lsp-pyright-python-executable-cmd . ,python-path))))
               (current-buffer))
        (insert "\n")))

    (message "[remote-venv] Set virtualenv: %s" venv-path)

    ;; Restart LSP if active
    (when (and (bound-and-true-p lsp-mode) (lsp-workspaces))
      (when (yes-or-no-p "Restart LSP to apply changes? ")
        (lsp-workspace-restart (car (lsp-workspaces)))))))

;;;###autoload
(defun remote-sync-detect-venv ()
  "Auto-detect and set virtualenv for current remote project."
  (interactive)
  (unless (projectile-project-p)
    (user-error "Not in a project"))
  (let* ((local-root (projectile-project-root))
         (remote-tramp (remote-sync--local-to-remote local-root)))
    (unless remote-tramp
      (user-error "Not in a remote-sync project"))
    (with-parsed-tramp-file-name remote-tramp parsed
      (if-let ((venv-path (remote-sync--detect-remote-venv parsed-host parsed-localname)))
          (remote-sync--set-project-venv local-root venv-path parsed-host)
        (if (yes-or-no-p "No virtualenv detected. Select manually? ")
            (remote-sync-select-venv)
          (message "[remote-venv] No virtualenv set"))))))

;;;###autoload
(defun remote-sync-show-venv ()
  "Show the current virtualenv for this remote project."
  (interactive)
  (let* ((local-root (projectile-project-root))
         (venv (gethash local-root remote-sync--project-venvs)))
    (if venv
        (message "[remote-venv] Current virtualenv: %s" venv)
      (message "[remote-venv] No virtualenv configured for this project"))))

;; Auto-detect venv when opening Python files in remote-sync projects
(defun remote-sync--maybe-setup-python-venv ()
  "Setup Python virtualenv for remote-sync projects."
  (when-let* ((local-root (ignore-errors (projectile-project-root)))
              (remote-tramp (remote-sync--local-to-remote local-root)))
    (with-parsed-tramp-file-name remote-tramp parsed
      ;; Check if we already have a venv configured
      (if-let ((stored-venv (gethash local-root remote-sync--project-venvs)))
          ;; Apply stored venv
          (let ((python-path (format "%s/bin/python" stored-venv)))
            (setq-local lsp-pyright-venv-path (file-name-directory stored-venv))
            (setq-local lsp-pyright-venv-directory (file-name-nondirectory stored-venv))
            (setq-local lsp-pyright-python-executable-cmd python-path)
            ;; Update modeline
            (remote-sync--update-modeline-venv stored-venv parsed-host))
        ;; Try auto-detection (but don't prompt)
        (when-let ((detected (remote-sync--detect-remote-venv parsed-host parsed-localname)))
          (let ((python-path (format "%s/bin/python" detected)))
            (puthash local-root detected remote-sync--project-venvs)
            (remote-sync--save-venv-registry)
            (setq-local lsp-pyright-venv-path (file-name-directory detected))
            (setq-local lsp-pyright-venv-directory (file-name-nondirectory detected))
            (setq-local lsp-pyright-python-executable-cmd python-path)
            ;; Update modeline
            (remote-sync--update-modeline-venv detected parsed-host)))))))

;; Note: Hooks are handled by config.el's projectile-pyenv-auto-activate
;; which delegates to remote-sync--maybe-setup-python-venv for remote projects

(provide 'remote-sync)
;;; remote-sync.el ends here
