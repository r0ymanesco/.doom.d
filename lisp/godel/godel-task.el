;;; godel-task.el --- Task model and store for godel -*- lexical-binding: t -*-

;;; Commentary:
;; A task is a plist (see PLAN.md, "Task model").  Tasks live in memory in
;; `godel-task--tasks' and are saved per repository in
;; <git-common-dir>/godel/tasks.eld, which every worktree of the repo shares
;; and git never commits.  `godel-task-repos-file' remembers which repos have
;; godel tasks so the board can show all of them.
;;
;; Every change goes through `godel-task-update', which saves the repo's
;; store and runs `godel-event-functions'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup godel nil
  "Agent development environment."
  :group 'tools
  :prefix "godel-")

(defcustom godel-directory "~/.godel/"
  "Directory for godel's own files (worktrees, caches, repo list)."
  :group 'godel
  :type 'directory)

(defcustom godel-worktree-root "~/.godel/worktrees/"
  "Directory under which task worktrees are created, as REPO/SLUG."
  :group 'godel
  :type 'directory)

(defvar godel-event-functions nil
  "Functions called after a task changes.
Each is called with two arguments: the task plist after the change and
a symbol naming the change (`created', `updated' or `deleted').")

(defconst godel-task-states
  '(setting-up needs-attention model-mismatch working needs-review idle stopped merged)
  "Known task states, in board order.")

(defvar godel-task--tasks (make-hash-table :test #'equal)
  "Task ID -> task plist, for every loaded repo.")

(defvar godel-task--loaded-repos nil
  "Repo roots whose stores have been read into `godel-task--tasks'.")

(defun godel-task-repos-file ()
  "File listing repos that have godel tasks."
  (expand-file-name "repos.eld" godel-directory))

;;;; Git helpers

(defun godel-task--git (dir &rest args)
  "Run git ARGS in DIR and return trimmed stdout, or nil on failure."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory (expand-file-name dir))))
      (when (zerop (apply #'process-file "git" nil (list t nil) nil args))
        (string-trim (buffer-string))))))

(defun godel-task-repo-root (dir)
  "Return the main worktree root of the git repo containing DIR, or nil.
Inside a linked worktree this is the root of the main checkout, so all
worktrees of a repo map to the same root."
  (when-let* ((common (godel-task--git dir "rev-parse" "--path-format=absolute"
                                       "--git-common-dir")))
    (file-name-as-directory
     (if (string-suffix-p "/.git" (directory-file-name common))
         (file-name-directory (directory-file-name common))
       ;; Bare repo: the common dir is the repo.
       common))))

(defun godel-task-store-dir (repo)
  "Directory holding godel's per-repo files for REPO."
  (let ((common (godel-task--git repo "rev-parse" "--path-format=absolute"
                                 "--git-common-dir")))
    (unless common (error "godel: %s is not a git repository" repo))
    (file-name-as-directory (expand-file-name "godel" common))))

(defun godel-task--store-file (repo)
  (expand-file-name "tasks.eld" (godel-task-store-dir repo)))

;;;; Reading and writing

(defun godel-task--read-file (file)
  "Read one Lisp object from FILE, or nil if it doesn't exist."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (ignore-errors (read (current-buffer))))))

(defun godel-task--write-file (file object)
  "Write OBJECT to FILE atomically."
  (make-directory (file-name-directory file) t)
  (let ((tmp (make-temp-name (concat file ".tmp"))))
    (with-temp-file tmp
      (let ((print-length nil) (print-level nil))
        (insert ";; -*- mode: lisp-data -*-\n")
        (prin1 object (current-buffer))
        (insert "\n")))
    (rename-file tmp file t)))

(defun godel-task--known-repos ()
  (godel-task--read-file (godel-task-repos-file)))

(defun godel-task--remember-repo (repo)
  (let ((repos (godel-task--known-repos)))
    (unless (member repo repos)
      (godel-task--write-file (godel-task-repos-file)
                              (append repos (list repo))))))

(defun godel-task-load-repo (repo)
  "Read REPO's task store into memory (once)."
  (setq repo (file-name-as-directory (expand-file-name repo)))
  (unless (member repo godel-task--loaded-repos)
    (push repo godel-task--loaded-repos)
    (when (file-directory-p repo)
      (dolist (task (godel-task--read-file (godel-task--store-file repo)))
        (puthash (plist-get task :id) task godel-task--tasks)))))

(defun godel-task-load-all ()
  "Load the stores of every repo godel knows about."
  (mapc #'godel-task-load-repo (godel-task--known-repos)))

(defun godel-task--save-repo (repo)
  (godel-task--write-file
   (godel-task--store-file repo)
   (cl-sort (cl-remove-if-not (lambda (task) (equal (plist-get task :repo) repo))
                              (hash-table-values godel-task--tasks))
            #'string< :key (lambda (task) (plist-get task :id)))))

;;;; Access

(defun godel-task-get (id)
  "Return the task with ID, or nil."
  (gethash id godel-task--tasks))

(defun godel-task-all ()
  "Return all loaded tasks, oldest first."
  (cl-sort (hash-table-values godel-task--tasks) #'<
           :key (lambda (task) (or (plist-get task :created) 0))))

(defun godel-task-by-session (session-id)
  "Return the task whose author session is SESSION-ID."
  (cl-find session-id (hash-table-values godel-task--tasks)
           :key (lambda (task) (plist-get task :session-id)) :test #'equal))

(defun godel-task-by-slug (slug)
  "Return the task with SLUG (also its Doom workspace name), or nil."
  (and slug (cl-find slug (hash-table-values godel-task--tasks)
                     :key (lambda (task) (plist-get task :slug)) :test #'equal)))

(defun godel-task-by-directory (dir)
  "Return the task whose worktree contains DIR."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (cl-find-if (lambda (task)
                  (when-let* ((wt (plist-get task :worktree)))
                    (string-prefix-p (file-name-as-directory (expand-file-name wt)) dir)))
                (hash-table-values godel-task--tasks))))

;;;; Changes

(defun godel-task-slug (title)
  "Turn TITLE into a branch-safe slug with a short random suffix."
  (let* ((base (downcase title))
         (base (replace-regexp-in-string "[^a-z0-9]+" "-" base))
         (base (string-trim base "-+" "-+"))
         (base (if (> (length base) 40)
                   (string-trim-right (substring base 0 40) "-+")
                 base))
         (base (if (string-empty-p base) "task" base)))
    (format "%s-%04x" base (random #x10000))))

(defun godel-task-create (&rest props)
  "Create a task from PROPS, save it and return it.
PROPS must include :title and :repo.  :id, :slug, :branch, :worktree,
:state and :created are filled in when missing."
  (let* ((repo (file-name-as-directory (expand-file-name (plist-get props :repo))))
         (slug (or (plist-get props :slug) (godel-task-slug (plist-get props :title))))
         (repo-name (file-name-nondirectory (directory-file-name repo)))
         (defaults (list :id slug :slug slug
                         :branch (concat "godel/" slug)
                         :worktree (file-name-as-directory
                                    (expand-file-name (concat repo-name "/" slug)
                                                      godel-worktree-root))
                         :state 'setting-up
                         :created (float-time)))
         (task (plist-put (godel-task--normalise (append props defaults)) :repo repo)))
    (godel-task-load-repo repo)
    (when (godel-task-get (plist-get task :id))
      (error "godel: task %s already exists" (plist-get task :id)))
    (puthash (plist-get task :id) task godel-task--tasks)
    (godel-task--remember-repo repo)
    (godel-task--save-repo repo)
    (run-hook-with-args 'godel-event-functions task 'created)
    task))

(defun godel-task--normalise (plist)
  "Return PLIST with each key kept once (first occurrence wins)."
  (let (seen out)
    (while plist
      (let ((k (pop plist)) (v (pop plist)))
        (unless (memq k seen)
          (push k seen)
          (setq out (nconc out (list k v))))))
    out))

(defun godel-task-update (id &rest props)
  "Set PROPS on task ID, save, run `godel-event-functions', return the task."
  (let ((task (or (godel-task-get id) (error "godel: no task %s" id))))
    (while props
      (setq task (plist-put task (pop props) (pop props))))
    (setq task (plist-put task :updated (float-time)))
    (puthash id task godel-task--tasks)
    (godel-task--save-repo (plist-get task :repo))
    (run-hook-with-args 'godel-event-functions task 'updated)
    task))

(defun godel-task-delete (id)
  "Remove task ID from the store."
  (when-let* ((task (godel-task-get id)))
    (remhash id godel-task--tasks)
    (godel-task--save-repo (plist-get task :repo))
    (run-hook-with-args 'godel-event-functions task 'deleted)
    task))

(provide 'godel-task)
;;; godel-task.el ends here
