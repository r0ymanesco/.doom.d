;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "r0ymanesco"
      user-mail-address "r0ymanesco@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-challenger-deep)


;; This fixes the pound key issue on UK MacOS keyboards
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))


;; Ensure env vars are available in Emacs
(use-package exec-path-from-shell
  :config
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-copy-envs '("OPENAI_API_KEY" "OPENROUTER_API_KEY"))
    (exec-path-from-shell-initialize)))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; pyenv
(require 'pyvenv)
(require 'pyenv-mode)

(defvar projectile-pyenv--manual-venvs (make-hash-table :test 'equal)
  "Hash table mapping project roots to manually set virtualenv paths.")

(defvar projectile-pyenv--last-project nil
  "Track the last project to avoid redundant activations.")

(defvar projectile-pyenv--skipped-projects (make-hash-table :test 'equal)
  "Hash table of projects where user declined to set venv manually.")

;; Helper to check if current project is a remote-sync project
(defun projectile-pyenv--remote-sync-project-p ()
  "Return remote TRAMP path if in a remote-sync project, nil otherwise."
  (when (and (bound-and-true-p remote-sync--projects)
             (projectile-project-p))
    (gethash (projectile-project-root) remote-sync--projects)))

(defun projectile-pyenv--restart-lsp-if-needed ()
  "Restart LSP if active in current buffer."
  (when (and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-workspace-restart)
             (lsp-workspaces))
    (let ((workspaces (lsp-workspaces)))
      (run-with-idle-timer 0.5 nil
                           (lambda ()
                             (dolist (ws workspaces)
                               (lsp-workspace-restart ws)))))))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name or poetry environment."
  (let* ((project (projectile-project-name))
         (project-root (projectile-project-root))
         (pyenv-versions (pyenv-mode-versions))
         (venv-found nil))
    
    ;; First, clean up any previously activated venv
    (when (bound-and-true-p pyvenv-virtual-env)
      (pyvenv-deactivate))
    (pyenv-mode-unset)
    
    (when project-root
      (cond
       ;; First check for poetry.lock and activate poetry env
       ((file-exists-p (expand-file-name "poetry.lock" project-root))
        (let* ((default-directory project-root)
               (poetry-output (shell-command-to-string "poetry env info -p 2>&1"))
               (poetry-env-dir (string-trim poetry-output)))
          (if (and poetry-env-dir
                   (not (string= poetry-env-dir ""))
                   (not (string-match-p "could not find" (downcase poetry-env-dir)))
                   (not (string-match-p "no virtual environment" (downcase poetry-env-dir)))
                   (file-directory-p poetry-env-dir))
              (progn
                (pyvenv-activate poetry-env-dir)
                (setq venv-found t)
                (message "[venv] Activated poetry env: %s" poetry-env-dir))
            ;; Fallback: search in poetry's cache directory
            (let* ((poetry-cache-dir (expand-file-name "~/.cache/pypoetry/virtualenvs"))
                   (matching-venv (when (file-directory-p poetry-cache-dir)
                                    (car (directory-files poetry-cache-dir t
                                                          (concat "^" (regexp-quote project) "-"))))))
              (if (and matching-venv (file-directory-p matching-venv))
                  (progn
                    (pyvenv-activate matching-venv)
                    (setq venv-found t)
                    (message "[venv] Activated poetry cache env: %s" matching-venv))
                (message "[venv] Poetry project found but no virtualenv. Output: %s" poetry-env-dir))))))
       
       ;; Check for .python-version file (pyenv local version)
       ((file-exists-p (expand-file-name ".python-version" project-root))
        (let* ((version-file (expand-file-name ".python-version" project-root))
               (pyenv-version (string-trim (with-temp-buffer
                                             (insert-file-contents version-file)
                                             (buffer-string))))
               (venv-path (expand-file-name pyenv-version "~/.pyenv/versions")))
          (if (file-directory-p venv-path)
              (progn
                (pyvenv-activate venv-path)
                (setq venv-found t)
                (message "[venv] Activated from .python-version: %s" venv-path))
            (message "[venv] .python-version specifies '%s' but path not found: %s"
                     pyenv-version venv-path))))
       
       ;; Check if there's a .venv directory in the project
       ((file-directory-p (expand-file-name ".venv" project-root))
        (let ((venv-path (expand-file-name ".venv" project-root)))
          (pyvenv-activate venv-path)
          (setq venv-found t)
          (message "[venv] Activated .venv: %s" venv-path)))
       
       ;; Try exact project name match in pyenv
       ((member project pyenv-versions)
        (let ((venv-path (expand-file-name project "~/.pyenv/versions")))
          (pyvenv-activate venv-path)
          (setq venv-found t)
          (message "[venv] Activated pyenv: %s" venv-path)))
       
       ;; Try poetry style naming (.venv-projectname)
       ((member (concat ".venv-" project) pyenv-versions)
        (let ((venv-path (expand-file-name (concat ".venv-" project) "~/.pyenv/versions")))
          (pyvenv-activate venv-path)
          (setq venv-found t)
          (message "[venv] Activated pyenv (.venv-style): %s" venv-path)))
       
       ;; Check for common virtualenv directories
       (t
        (let ((common-venv-dirs '("venv" "env" ".env" "virtualenv")))
          (catch 'found
            (dolist (venv-dir common-venv-dirs)
              (let ((venv-path (expand-file-name venv-dir project-root)))
                (when (file-directory-p venv-path)
                  (pyvenv-activate venv-path)
                  (setq venv-found t)
                  (message "[venv] Activated %s: %s" venv-dir venv-path)
                  (throw 'found t)))))))))
    
    ;; Restart LSP if venv was activated
    (when venv-found
      (projectile-pyenv--restart-lsp-if-needed))
    
    ;; If no environment was found, optionally prompt
    (unless venv-found
      (message "[venv] No Python environment found for project: %s" project)
      (unless (gethash project-root projectile-pyenv--skipped-projects)
        (if (y-or-n-p "No Python environment found. Select manually?")
            (call-interactively 'projectile-pyenv-set-manual-venv)
          (puthash project-root t projectile-pyenv--skipped-projects))))))

(defun projectile-pyenv-set-manual-venv ()
  "Manually set a virtualenv. Delegates to remote-sync for remote projects."
  (interactive)
  (if (projectile-pyenv--remote-sync-project-p)
      ;; Remote project - use remote-sync selection
      (if (fboundp 'remote-sync-select-venv)
          (remote-sync-select-venv)
        (user-error "remote-sync not loaded"))
    ;; Local project - use local selection
    (let* ((venv-path (read-directory-name "Path to virtualenv: "
                                           (or (projectile-project-root) default-directory)
                                           nil t))
           (expanded-path (expand-file-name venv-path))
           (project-root (projectile-project-root)))
      (if (and (file-directory-p expanded-path)
               (or (file-exists-p (expand-file-name "bin/python" expanded-path))
                   (file-exists-p (expand-file-name "Scripts/python.exe" expanded-path))))
          (progn
            (when (bound-and-true-p pyvenv-virtual-env)
              (pyvenv-deactivate))
            (pyenv-mode-unset)
            (pyvenv-activate expanded-path)
            (setq projectile-pyenv--last-project project-root)
            ;; Store in hash table
            (puthash project-root expanded-path projectile-pyenv--manual-venvs)
            ;; Restart LSP
            (projectile-pyenv--restart-lsp-if-needed)
            (message "[venv] Manually activated: %s" expanded-path))
        (error "[venv] Invalid virtualenv path: %s (no python executable found)" expanded-path)))))

(defun projectile-pyenv-show-current-venv ()
  "Display the currently active Python virtualenv path."
  (interactive)
  (if (projectile-pyenv--remote-sync-project-p)
      ;; Remote project - show remote venv
      (if (fboundp 'remote-sync-show-venv)
          (remote-sync-show-venv)
        (message "[venv] remote-sync not loaded"))
    ;; Local project - show local venv
    (if (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
        (message "[venv] Current virtualenv: %s" pyvenv-virtual-env)
      (message "[venv] No Python virtualenv currently active"))))

(defun projectile-pyenv-auto-activate ()
  "Auto-activate Python virtualenv when opening a Python file in a new project.
Delegates to remote-sync for remote projects."
  (when (and (projectile-project-p)
             (not (equal (projectile-project-root)
                         projectile-pyenv--last-project)))
    (let ((project-root (projectile-project-root)))
      (setq projectile-pyenv--last-project project-root)
      ;; Check if this is a remote-sync project
      (if (projectile-pyenv--remote-sync-project-p)
          ;; Remote project - use remote-sync venv management
          (when (fboundp 'remote-sync--maybe-setup-python-venv)
            (remote-sync--maybe-setup-python-venv))
        ;; Local project - use local venv management
        (let ((manual-venv (gethash project-root projectile-pyenv--manual-venvs)))
          (if manual-venv
              ;; Restore manually set venv
              (progn
                (when (bound-and-true-p pyvenv-virtual-env)
                  (pyvenv-deactivate))
                (pyvenv-activate manual-venv)
                (projectile-pyenv--restart-lsp-if-needed)
                (message "[venv] Restored manual venv: %s" manual-venv))
            ;; Auto-detect venv
            (projectile-pyenv-mode-set)))))))

(add-hook 'python-mode-hook #'projectile-pyenv-auto-activate)
(add-hook 'python-ts-mode-hook #'projectile-pyenv-auto-activate)

;; Keybinding to manually set or show virtualenv path
(map! :leader
      :prefix "p"
      :desc "Set virtualenv path" "v" #'projectile-pyenv-set-manual-venv
      :desc "Show current venv" "V" #'projectile-pyenv-show-current-venv)


;; Magit
(require 'magit-todos)
(magit-todos-mode)


;; Aidermacs
(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  (unless (getenv "OPENROUTER_API_KEY")
    (warn "OPENROUTER_API_KEY not set in environment")
    )
  (unless (getenv "OPENAI_API_KEY")
    (warn "OPENAI_API_KEY not set in environment")
    )
  (setq aidermacs-vterm-multiline-newline-key "S-<return>")
  ;; Enable file watching
  (setq aidermacs-watch-files t)
  ;; Using reasoning
  (setq aidermacs-extra-args '("--reasoning-effort" "medium"))
  :custom
  ; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "gpt-5.2")
  (aidermacs-architect-model "gpt-5.2")
  (aidermacs-editor-model "gpt-5.2")
  (aidermacs-weak-model "gpt-5.2")
  )


;; Remote-sync: seamless remote project editing with Mutagen
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))
(require 'remote-sync)

;; TRAMP optimizations for remote-sync
(after! tramp
  (setq tramp-verbose 1)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='~/.ssh/sockets/%%r@%%h-%%p' -o ControlPersist=600"))


;; Claude code
(use-package! claude-code-ide
  :config
  (claude-code-ide-emacs-tools-setup)  ; Optionally enable Emacs MCP tools
  (map! :map vterm-mode-map
        "S-<return>" #'claude-code-ide-insert-newline)
  (advice-add 'claude-code-ide-insert-at-mentioned :after
              (lambda (&rest _) (claude-code-ide-switch-to-buffer))))
(map! :leader
      :prefix ("c" . "Claude Code")
      :desc "Menu" "m" #'claude-code-ide-menu
      :desc "Start Claude Code for project" "s" #'claude-code-ide
      :desc "Continue conversation" "C" #'claude-code-ide-continue
      :desc "Resume conversation" "r" #'claude-code-ide-resume
      :desc "Stop for project" "q" #'claude-code-ide-stop
      :desc "Switch to project session buffer" "b" #'claude-code-ide-switch-to-buffer
      :desc "List sessions" "l" #'claude-code-ide-list-sessions
      :desc "Toggle window" "t" #'claude-code-ide-toggle
      :desc "Toggle recent" "T" #'claude-code-ide-toggle-recent
      :desc "Send prompt" "p" #'claude-code-ide-send-prompt
      :desc "Send selection (@)" "a" #'claude-code-ide-insert-at-mentioned
      :desc "Send escape" "e" #'claude-code-ide-send-escape
      :desc "Check status" "S" #'claude-code-ide-check-status
      :desc "Show debug" "d" #'claude-code-ide-show-debug
      :desc "Clear debug" "D" #'claude-code-ide-clear-debug
      :desc "Setup Emacs tools" "E" #'claude-code-ide-emacs-tools-setup
) 


;; Ranger
(ranger-override-dired-mode t)
(map! :leader :prefix "o" :desc "ranger" "D" 'ranger)


;; Org
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org")
;; (setq org-default-notes-file "~/Dropbox/org/notes.org")
(setq org-log-done 'time)
(setq org-agenda-files (list "~/Dropbox/org/notebook/journal"))
(setq org-id-extra-files (directory-files-recursively "~/Dropbox/org/notebook/notes" "org"))
(setq org-id-extra-files (directory-files-recursively "~/Dropbox/org/bibliography/notes" "org"))

;; (after! org-capture
;;   (add-to-list 'org-capture-templates
;;                '("u" "Quick note" entry
;;                  (file+headline "~/Dropbox/org/notes.org" "Quick Notes")
;;                  "* %t %?\n%a" :kill-buffer t)
;;                )
;; )

;; (after! org-capture
;;   (add-to-list 'org-capture-templates
;;                '("v" "Quick todo" entry
;;                  (file+headline "~/Dropbox/org/notes.org" "Quick Notes")
;;                  "* TODO %t %?" :kill-buffer t)
;;                )
;; )

(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "STRT(s)" "HOLD(h)" "MEETING(m)" "READ(r)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
        )
)
;; (setq org-todo-keyword-faces
;;       '(("TODO" . "green") ("STRT" . "blue") ("HOLD" . "orange") ("MEETING" . "yellow")
;;         ("KILL" . "red")))

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; doesn't work for checkboxes
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
(add-hook 'org-trigger-hook 'save-buffer)

(defmacro ignore-args (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))
(advice-add 'org-schedule :after (ignore-args #'org-save-all-org-buffers))

;; (add-hook 'org-after-todo-state-change-hook 'save-buffer) -> doesn't actually save the file for some reason
;; Source: https://orgmode.org/manual/Breaking-Down-Tasks.html

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  )

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks t
           org-appear-autoentities t
           org-appear-autosubmarkers t))


;; ox-hugo
(with-eval-after-load 'org
  (map! :desc "org-hugo-export-wim-to-md" "C-c H" 'org-hugo-export-wim-to-md))


;; latex
;; (latex-preview-pane-enable)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)

;; Ebib
(map! :leader :prefix "o" :desc "Ebib" "B" 'ebib)
(use-package ebib
;; :bind (("C-c e" . ebib))
:ensure t
:config
(setq ebib-index-columns
(quote(
("Entry Key" 30 t)
("Author/Editor" 20 nil)
("Year" 6 t)
("Title" 60 t)
("keywords" 20 t))))
(setq ebib-bibtex-dialect 'biblatex
      ebib-preload-bib-files '("~/Dropbox/org/bibliography/bib/main.bib")
      ebib-file-search-dirs '("~/Sync/papers/pdf")
      ebib-notes-directory "~/Dropbox/org/bibliography/notes"
      ebib-use-timestamp t
      ebib-timestamp-format "%d.%m.%Y"
      ebib-index-window-size 50
      ebib-keywords (expand-file-name "~/Dropbox/org/bibliography/keywords.txt")
      ebib-keywords-add-new-to-canonical 0
      ebib-keywords-save-on-exit t
      ebib-reading-list-file "~/Dropbox/org/notebook/journal/reading_list.org"
      ebib-reading-list-todo-marker "READ")
(if (eq system-type 'darwin)
        (add-to-list 'ebib-file-associations '("pdf" . "open %s"))
        (add-to-list 'ebib-file-associations '("pdf" . nil)))
(advice-add 'bibtex-generate-autokey :around
              (lambda (orig-func &rest args)
                (replace-regexp-in-string ":" "" (apply orig-func args))))
(map! :map ebib-index-mode-map "B" #'ebib-biblio-import-doi)
(map! :map ebib-index-mode-map "F f" #'ebib-import-file)
(map! :map ebib-index-mode-map "C-x S" #'ebib-edit-strings)
(map! :map ebib-index-mode-map "C-x B" #'biblio-lookup)
(map! :map ebib-strings-mode-map "C-x a" #'ebib-add-string))
;; (setq ebib-window-vertical-split t)
;; (setq ebib-file-associations
;;       '(("pdf" . "open -a Skim %s")))

(require 'ebib-biblio)
(with-eval-after-load 'biblio
  (define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import))
;; (with-eval-after-load 'ebib
;;   (define-key ebib-index-mode-map (kbd "B") #'ebib-biblio-import-doi)
;;   (define-key ebib-index-mode-map (kbd "F f") #'ebib-import-file)
;;   (define-key ebib-index-mode-map (kbd "C-x S") #'ebib-edit-strings)
;;   (define-key ebib-strings-mode-map (kbd "C-x a") #'ebib-add-string))

;; (defun acm-pdf-url (doi)
;;   "Retrieve a DOI pdf from the ACM."
;;   (concat "https://dl.acm.org/doi/pdf/" doi))

;; (defun ieee-pdf-url (doi)
;;   "Retrieve a DOI pdf from the IEEE."
;;   (when (string-match "\\.\\([0-9]*\\)$" doi)
;;     (let ((doi-bit (match-string 1 doi)))
;;       (concat "https://ieeexplore.ieee.org/stampPDF/getPDF.jsp?tp=&arnumber=" doi-bit "&ref="))))

;; (defun springer-pdf-url (doi)
;;   "Retrieve a DOI pdf from the Springer."
;;   (concat "https://link.springer.com/content/pdf/" doi ".pdf"))

;; (defun arxiv-pdf-url (eprint)
;;   "Download an arXiv pdf based on it's EPRINT number."
;;   (concat "https://arxiv.org/pdf/" eprint ".pdf"))

;; (defun download-pdf-from-doi (key &optional doi publisher eprint journal organization url)
;;   "Download pdf from DOI with KEY name."
;;   (let ((pub  (or publisher ""))
;;         (epr  (or eprint ""))
;;         (jour (or journal ""))
;;         (org  (or organization ""))
;;         (link (or url "")))
;;     (url-copy-file (cond
;;                     ((not doi) link)
;;                     ((or (string-match "ACM" (s-upcase pub))
;;                          (string-match "association for computing machinery" (s-downcase pub)))
;;                      (acm-pdf-url doi))
;;                     ((string-match "arxiv" (s-downcase pub))
;;                      (arxiv-pdf-url epr))
;;                     ((or (string-match "IEEE" (s-upcase pub))
;;                          (string-match "IEEE" (s-upcase jour))
;;                          (string-match "IEEE" (s-upcase org)))
;;                      (ieee-pdf-url doi))
;;                     ((string-match "springer" (s-downcase pub))
;;                      (springer-pdf-url doi))
;;                     (t (error "Cannot possibly find the PDF any other way")))
;;                    (concat (car ebib-file-search-dirs) "/" key ".pdf"))))

;; (defun download-pdf-from-link (link key)
;;   (url-copy-file link
;;                  (concat (car ebib-file-search-dirs) "/" key ".pdf")))

;; (defun download-pdf-from-downloads (key)
;;   (copy-file (concat "~/Downloads/" key ".pdf")
;;              (concat (car ebib-file-search-dirs) "/" key ".pdf") t))

;; (defun get-bib-from-doi (doi)
;;   "Get the bibtex from DOI."
;;   (shell-command (concat "curl -L -H \"Accept: application/x-bibtex; charset=utf-8\" "
;;                          "https://doi.org/" doi)))

;; (defun ebib-download-pdf-from-doi ()
;;   "Download a PDF for the current entry."
;;   (interactive)
;;   (let* ((key (ebib--get-key-at-point))
;;          (doi (ebib-get-field-value "doi" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (publisher (ebib-get-field-value "publisher" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (eprinttype (ebib-get-field-value "eprinttype" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (eprint (ebib-get-field-value "eprint" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (journal (ebib-get-field-value "journal" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (journaltitle (ebib-get-field-value "journaltitle" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (organization (ebib-get-field-value "organization" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (url (ebib-get-field-value "url" key ebib--cur-db 'noerror 'unbraced 'xref)))
;;     (unless key
;;       (error "[Ebib] No key assigned to entry"))
;;     (download-pdf-from-doi key doi (or publisher eprinttype) eprint (or journal journaltitle) organization url)))

;; (defun ebib-check-file ()
;;   "Download a PDF for the current entry."
;;   (interactive)
;;   (let ((key (ebib--get-key-at-point)))
;;     (unless (file-exists-p (concat (car ebib-file-search-dirs) "/" key ".pdf"))
;;       (error "[Ebib] No PDF found"))))


;; Zettelkasten
;; (use-package org-zettelkasten
;;   :ensure t
;;   :config
;;   (add-hook 'org-mode-hook #'org-zettelkasten-mode))
;; (setq org-zettelkasten-directory "~/Dropbox/org")

;; (use-package zettelkasten
;;   :ensure t
;;   :config
;;   (zettelkasten-mode t))

;; (defun org-zettelkasten-search-current-id ()
;;     "Use `counsel-rg' to search for the current ID in all files."
;;     (interactive)
;;     (let ((current-id (org-entry-get nil "CUSTOM_ID")))
;;       (counsel-rg (concat "#" current-id) org-zettelkasten-directory "-g *.org" "ID: ")))

;; (defun r0ymanesco/incr-id (ident)
;;   (let* ((ident-list (append nil ident nil))
;;          (last-ident (last ident-list)))
;;     (setcar last-ident (+ (car last-ident) 1))
;;     (concat ident-list)))

;; (defun r0ymanesco/branch-id (ident)
;;   (if (string-match-p ".*[0-9]$" ident)
;;       (concat ident "a")
;;     (concat ident "1")))

;; (defun r0ymanesco/org-zettelkasten-create (incr newheading)
;;   (let* ((current-id (org-entry-get nil "CUSTOM_ID"))
;;          (next-id (funcall incr current-id)))
;;     (funcall newheading)
;;     (org-set-property "CUSTOM_ID" next-id)))

;; (defun org-zettelkasten-create-heading ()
;;   (r0ymanesco/org-zettelkasten-create
;;    'r0ymanesco/incr-id 'org-insert-heading))

;; (defun org-zettelkasten-create-subheading ()
;;   (r0ymanesco/org-zettelkasten-create
;;    'r0ymanesco/branch-id '(lambda () (org-insert-subheading ""))))

;; (defun org-zettelkasten-create-dwim ()
;;   (interactive)
;;   (let ((current-point (save-excursion
;;                          (org-back-to-heading)
;;                          (point)))
;;         (next-point (save-excursion
;;                       (org-forward-heading-same-level 1 t)
;;                       (point))))
;;     (if (= current-point next-point)
;;         (org-zettelkasten-create-heading)
;;         (org-zettelkasten-create-subheading))))


;; Tabs
(use-package centaur-tabs
  :config
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-c h" . centaur-tabs-backward)
  ("C-c l" . centaur-tabs-forward)
  ("C-c d" . centaur-tabs--kill-this-buffer-dont-ask)
  ("C-c n" . centaur-tabs--create-new-tab)
  )


;; Rust
(add-to-list 'exec-path "~/.cargo/bin")

;; (with-eval-after-load 'rust-mode
;;   (define-key rust-mode-map (kbd "C-c C-r") 'my-cargo-run))

;; custom method to allow keyboard input
;; doesn't work...
(defun my-cargo-run ()
  "Build and run Rust code."
  (interactive)
  (rustic-cargo-run)
  (let (
      (orig-win (selected-window))
      (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible))
    )
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (select-window orig-win)
  )
)


;; ssh-deploy
;; - prefix = C-c C-z, f = forced upload, u = upload, d = download, x = diff, t = terminal, b = browse, h = shell
;; Need to setup passwordless copy to remote and project .dir-locals.el config
;; NOTE: hydra bindings don't work
;; NOTE: removed after-save hook to avoid non-project save errors, seems to work w/o. add back if problems occur
(use-package ssh-deploy
       :ensure t
       :hook ((find-file . ssh-deploy-find-file))
              ;; (after-save . ssh-deploy-after-save)
       :config
       (ssh-deploy-line-mode) ;; If you want mode-line feature
       (ssh-deploy-add-menu)) ;; If you want menu-bar feature


;; (define-key evil-motion-state-map (kbd "C-e") nil)
;; (define-key smartparens-mode-map (kbd "C-e") #'sp-up-sexp)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
