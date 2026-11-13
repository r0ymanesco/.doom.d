;;; godel-new.el --- Creating godel tasks -*- lexical-binding: t -*-

;;; Commentary:
;; `godel-new-task' opens a transient to choose base branch, model, effort
;; and permission mode, then a compose buffer (or the minibuffer) for the
;; prompt.  Submitting creates the task and runs, asynchronously:
;; worktree + setup script -> proxy guard -> Claude launch -> prompt.
;; See PLAN.md, "New task".

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'godel-task)
(require 'godel-models)
(require 'godel-worktree)
(require 'godel-agent)
(require 'godel-compose)
(require 'godel-status)

(defvar godel-new--repo nil "Repo the open new-task transient is for.")

;;;; Readers

(defun godel-new--unavailable-p (m)
  "Non-nil if model M can't be used because the pin check failed."
  (and (not (godel-models-router-p m)) (godel-models-pin-broken-p)))

(defun godel-new--model-label (m)
  (format "%-22s %s%s" (plist-get m :name) (plist-get m :description)
          (if (godel-new--unavailable-p m) "  [unavailable: model pin not working]" "")))

(defun godel-new--read-model (prompt _initial _history)
  "Read a model value, showing names and descriptions like /model."
  (let* ((models (godel-models))
         (choices (mapcar (lambda (m) (cons (godel-new--model-label m) m)) models))
         (default (cl-find godel-default-model models
                           :key (lambda (m) (plist-get m :value)) :test #'equal))
         (pick (completing-read prompt (mapcar #'car choices) nil t nil nil
                                (and default (godel-new--model-label default)))))
    (when (godel-new--unavailable-p (cdr (assoc pick choices)))
      (user-error "godel: %s is unavailable: %s" (plist-get (cdr (assoc pick choices)) :name)
                  (cdr godel-models-pin-status)))
    (plist-get (cdr (assoc pick choices)) :value)))

(defun godel-new--current-model ()
  (or (godel-models-find (or (transient-arg-value "--model=" (transient-get-value))
                             godel-default-model))
      (godel-models-find godel-default-model)))

(defun godel-new--router-selected-p ()
  (godel-models-router-p (godel-new--current-model)))

(defun godel-new--read-effort (prompt _initial _history)
  (completing-read prompt (or (plist-get (godel-new--current-model) :efforts)
                              '("low" "medium" "high" "xhigh" "max"))
                   nil t nil nil godel-default-effort))

(defun godel-new--branches (repo)
  (let ((default-directory repo))
    (process-lines "git" "for-each-ref" "--format=%(refname:short)" "refs/heads/")))

(defun godel-new--read-base (prompt _initial _history)
  (completing-read prompt (godel-new--branches godel-new--repo) nil t))

(defun godel-new--default-base (repo)
  (or (godel-task--git repo "symbolic-ref" "--short" "HEAD") "HEAD"))

;;;; Transient

(transient-define-prefix godel-new-task-menu ()
  "Create a godel task."
  ;; Re-check :inapt-if after each change, so effort is enabled as soon as a
  ;; pinned model is picked (and greyed out again for Not Diamond).
  :refresh-suffixes t
  :value (lambda ()
           (list (concat "--base=" (godel-new--default-base godel-new--repo))
                 (concat "--model=" godel-default-model)
                 (concat "--effort=" godel-default-effort)))
  [:description
   (lambda () (format "New task in %s" (abbreviate-file-name godel-new--repo)))
   ("-b" "base branch" "--base=" :reader godel-new--read-base :always-read t)
   ("-m" "model" "--model=" :reader godel-new--read-model :always-read t)
   ("-e" "effort" "--effort=" :reader godel-new--read-effort :always-read t
    :inapt-if godel-new--router-selected-p
    :inapt-face transient-inapt-suffix)
   ("-p" "permission mode" "--permission-mode="
    :choices ("default" "acceptEdits" "plan" "auto"))]
  ["Prompt"
   ("c" "compose" godel-new-task-compose)
   ("RET" "quick (minibuffer)" godel-new-task-quick)])

;;;###autoload
(defun godel-new-task (&optional repo)
  "Create a task in REPO (default: the repo at `default-directory')."
  (interactive)
  (setq godel-new--repo
        (or repo
            (godel-task-repo-root default-directory)
            (godel-task-repo-root (read-directory-name "godel: repository: "))
            (user-error "godel: not a git repository")))
  (transient-setup 'godel-new-task-menu))

(defun godel-new--args ()
  (let ((args (transient-args 'godel-new-task-menu)))
    (list :base (transient-arg-value "--base=" args)
          :model (or (transient-arg-value "--model=" args) godel-default-model)
          :effort (transient-arg-value "--effort=" args)
          :permission-mode (or (transient-arg-value "--permission-mode=" args)
                               godel-default-permission-mode))))

(defun godel-new-task-compose ()
  "Write the prompt in a compose buffer.  The first line is the title."
  (interactive)
  (let ((args (godel-new--args)) (repo godel-new--repo))
    (godel-compose repo 'task
                   (format "New task (%s) · first line = title"
                           (plist-get (godel-models-find (plist-get args :model)) :name))
                   (lambda (text) (godel-new--create repo args text)))))

(defun godel-new-task-quick ()
  "Read the prompt in the minibuffer."
  (interactive)
  (let ((args (godel-new--args)) (repo godel-new--repo))
    (godel-new--create repo args (read-string "godel prompt: "))))

;;;; Creating

(defun godel-new--uuid ()
  "A random (version 4) UUID."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random #x10000) (random #x10000) (random #x10000) (random #x1000)
          (logior #x8000 (random #x4000))
          (random #x10000) (random #x10000) (random #x10000)))

(defun godel-new--split (text)
  "Split TEXT into (TITLE . PROMPT).  The first line is the title.
If there is nothing after it, the title is also the prompt."
  (let* ((lines (split-string (string-trim text) "\n"))
         (title (string-trim (string-remove-prefix "#" (car lines))))
         (rest (string-trim (string-join (cdr lines) "\n"))))
    (cons (if (> (length title) 60) (concat (substring title 0 59) "…") title)
          (if (string-empty-p rest) (string-trim (car lines)) rest))))

(defun godel-new--create (repo args text)
  "Create the task in REPO from transient ARGS and prompt TEXT, then start it."
  (when (string-empty-p (string-trim text)) (user-error "godel: empty prompt"))
  (pcase-let* ((`(,title . ,prompt) (godel-new--split text))
               (task (apply #'godel-task-create
                            :title title :repo repo
                            :session-id (godel-new--uuid)
                            :prompt prompt
                            :pending-prompt prompt
                            :last-event "creating the worktree"
                            args))
               (id (plist-get task :id)))
    (message "godel: creating %s" id)
    (godel-worktree-create
     task
     (lambda (err)
       (if err
           (godel-task-update id :state 'stopped :last-event err)
         (godel-agent-start (godel-task-get id)))))
    task))

;;;; Importing an existing worktree

(defcustom godel-import-exclude-regexp "/\\.paradev\\(-[^/]*\\)?/"
  "Worktrees whose path matches this are never offered for import.
By default Paradev's worktrees, which godel stays separate from."
  :group 'godel
  :type 'regexp)

(defun godel-new--worktrees (repo)
  "REPO's worktrees as (PATH . BRANCH), main checkout and detached ones excluded."
  (let ((default-directory repo) entries path)
    (dolist (line (process-lines "git" "worktree" "list" "--porcelain"))
      (cond ((string-prefix-p "worktree " line) (setq path (substring line 9)))
            ((string-prefix-p "branch refs/heads/" line)
             (push (cons (file-name-as-directory path) (substring line 18)) entries))))
    (cl-remove-if (lambda (e) (equal (expand-file-name (car e)) (expand-file-name repo)))
                  (nreverse entries))))

(defun godel-new-importable-worktrees (repo)
  "REPO's worktrees that can become tasks: not already one, not excluded."
  (cl-remove-if (lambda (e)
                  (or (string-match-p godel-import-exclude-regexp (car e))
                      (cl-find (expand-file-name (car e)) (godel-task-all)
                               :key (lambda (task) (expand-file-name (plist-get task :worktree)))
                               :test #'equal)))
                (godel-new--worktrees repo)))

(defun godel-new-import (repo path branch base title)
  "Make a task of REPO's existing worktree at PATH (on BRANCH, from BASE)."
  (let* ((name (file-name-nondirectory (directory-file-name path)))
         (slug (if (or (godel-task-get name) (godel-task-by-slug name)) (godel-task-slug name) name))
         (task (godel-task-create :title title :repo repo :id slug :slug slug
                                  :worktree (file-name-as-directory (expand-file-name path))
                                  :branch branch :base base
                                  :model godel-default-model :effort godel-default-effort
                                  :session-id (godel-new--uuid)
                                  :state 'idle :last-event "imported; R starts a session")))
    (godel-status-check-changes
     (plist-get task :id)
     (lambda (changed stat)
       (when (godel-task-get (plist-get task :id))
         (godel-task-update (plist-get task :id) :state (if changed 'needs-review 'idle)
                            :diffstat stat))))
    task))

;;;###autoload
(defun godel-import-worktree (&optional repo)
  "Make an existing git worktree of REPO (default: this repo) a godel task."
  (interactive)
  (let* ((repo (or repo (godel-task-repo-root default-directory)
                   (godel-task-repo-root (read-directory-name "godel: repository: "))
                   (user-error "godel: not a git repository")))
         (_ (godel-task-load-repo repo))
         (candidates (or (godel-new-importable-worktrees repo)
                         (user-error "godel: no worktrees of %s to import" (abbreviate-file-name repo))))
         (labels (mapcar (lambda (e) (cons (format "%s  (%s)" (abbreviate-file-name (car e)) (cdr e)) e))
                         candidates))
         (pick (cdr (assoc (completing-read "Import worktree: " labels nil t) labels)))
         (base (completing-read "Base branch: " (godel-new--branches repo) nil t nil nil
                                (godel-new--default-base repo)))
         (title (read-string "Title: " (cdr pick))))
    (message "godel: imported %s" (plist-get (godel-new-import repo (car pick) (cdr pick) base title) :id))))

(provide 'godel-new)
;;; godel-new.el ends here
