;;; godel-worktree.el --- Worktree lifecycle for godel tasks -*- lexical-binding: t -*-

;;; Commentary:
;; Creates and removes task worktrees asynchronously, and runs the repo's
;; setup/teardown commands from .godel.toml (PLAN.md, "Worktree lifecycle").
;;
;; .godel.toml is read from the repo's main checkout.  Only a small subset of
;; TOML is supported: top-level `key = "string"' or `key = ["a", "b"]' on one
;; line, and # comments.  Keys used: setup, teardown.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'godel-task)

(defun godel-worktree-read-config (repo)
  "Return REPO's .godel.toml as an alist of (KEY . VALUE), or nil."
  (let ((file (expand-file-name ".godel.toml" repo)) config)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (line (split-string (buffer-string) "\n"))
          (setq line (replace-regexp-in-string "[ \t]+#[^\"]*\\'" "" line))
          (when (string-match "\\`[ \t]*\\([A-Za-z0-9_-]+\\)[ \t]*=[ \t]*\\(.*?\\)[ \t]*\\'"
                              line)
            (let ((key (intern (match-string 1 line)))
                  (value (ignore-errors
                           (json-parse-string (match-string 2 line) :array-type 'list))))
              (when value (push (cons key value) config)))))))
    (nreverse config)))

(defun godel-worktree--commands (repo key)
  "Return REPO's commands for KEY as a list of shell strings."
  (let ((value (alist-get key (godel-worktree-read-config repo))))
    (if (stringp value) (list value) value)))

(defun godel-worktree--run (name dir command callback &optional log-buffer)
  "Run shell COMMAND in DIR; call CALLBACK with the exit status.
Output goes to LOG-BUFFER (created if needed)."
  (let ((default-directory (file-name-as-directory dir))
        (buf (get-buffer-create (or log-buffer (format " *godel-%s*" name)))))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "$ %s\n" (if (listp command) (string-join command " ") command))))
    (make-process
     :name (concat "godel-" name) :buffer buf :noquery t :connection-type 'pipe
     :command (if (listp command) command (list shell-file-name shell-command-switch command))
     :sentinel (lambda (p _)
                 (unless (process-live-p p)
                   (funcall callback (process-exit-status p)))))))

(defun godel-worktree--run-all (name dir commands callback log-buffer)
  "Run COMMANDS in DIR one after another; stop at the first failure.
Calls CALLBACK with nil on success or an error string."
  (if (null commands)
      (funcall callback nil)
    (godel-worktree--run
     name dir (car commands)
     (lambda (status)
       (if (zerop status)
           (godel-worktree--run-all name dir (cdr commands) callback log-buffer)
         (funcall callback (format "`%s' failed (exit %d); see %s"
                                   (car commands) status log-buffer))))
     log-buffer)))

(defun godel-worktree-log-buffer (task)
  (format "*godel-setup: %s*" (plist-get task :id)))

(defun godel-worktree-create (task callback)
  "Create TASK's worktree and branch, then run the setup commands.
Calls CALLBACK with nil on success or an error string."
  (let* ((repo (plist-get task :repo))
         (path (directory-file-name (plist-get task :worktree)))
         (log (godel-worktree-log-buffer task)))
    (make-directory (file-name-directory path) t)
    (godel-worktree--run
     "worktree" repo
     (list "git" "worktree" "add" "-b" (plist-get task :branch) path (plist-get task :base))
     (lambda (status)
       (if (not (zerop status))
           (funcall callback (format "git worktree add failed; see %s" log))
         (godel-worktree--run-all "setup" path (godel-worktree--commands repo 'setup)
                                  callback log)))
     log)))

(defun godel-worktree-check (task callback)
  "Call CALLBACK with (UNCOMMITTED . UNPUSHED) counts for TASK's worktree.
UNCOMMITTED counts changed and untracked files.  UNPUSHED counts commits
on the branch that are neither upstream nor on the base."
  (let ((default-directory (plist-get task :worktree))
        (buf (generate-new-buffer " *godel-check*")))
    (if (not (file-directory-p default-directory))
        (funcall callback '(0 . 0))
      (make-process
       :name "godel-check" :buffer buf :noquery t :connection-type 'pipe
       :command (list "sh" "-c"
                      "echo \"$(git status --porcelain | wc -l)\"
if git rev-parse -q --verify '@{u}' >/dev/null 2>&1; then git rev-list --count '@{u}..HEAD'
else git rev-list --count \"$1..HEAD\"; fi"
                      "godel" (plist-get task :base))
       :sentinel (lambda (p _)
                   (unless (process-live-p p)
                     (let ((nums (mapcar #'string-to-number
                                         (split-string (with-current-buffer buf (buffer-string))
                                                       "\n" t "[ \t]+"))))
                       (kill-buffer buf)
                       (funcall callback (cons (or (nth 0 nums) 0) (or (nth 1 nums) 0))))))))))

(defun godel-worktree-remove (task callback)
  "Run teardown commands and remove TASK's worktree; keep the branch.
Calls CALLBACK with nil on success or an error string."
  (let* ((repo (plist-get task :repo))
         (path (directory-file-name (plist-get task :worktree)))
         (log (godel-worktree-log-buffer task)))
    (if (not (file-directory-p path))
        (funcall callback nil)
      (godel-worktree--run-all
       "teardown" path (godel-worktree--commands repo 'teardown)
       (lambda (err)
         (if err
             (funcall callback err)
           (godel-worktree--run
            "worktree" repo (list "git" "worktree" "remove" "--force" path)
            (lambda (status)
              (funcall callback (unless (zerop status)
                                  (format "git worktree remove failed; see %s" log))))
            log)))
       log))))

(provide 'godel-worktree)
;;; godel-worktree.el ends here
