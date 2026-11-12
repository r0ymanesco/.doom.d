;;; godel-board.el --- The godel board -*- lexical-binding: t -*-

;;; Commentary:
;; A magit-section buffer listing every task, grouped by state.  It redraws
;; itself (debounced) whenever a task changes.  See PLAN.md, "Board".
;;
;;   RET  open the task's workspace     n  new task
;;   TAB  show details                  m  message the agent
;;   R    restart/resume (new model?)   x  tear down (keeps the branch)
;;   gr   refresh                       q  quit

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit-section)
(require 'godel-task)
(require 'godel-models)
(require 'godel-agent)
(require 'godel-worktree)

(declare-function godel-new-task "godel-new" (&optional repo))
(declare-function godel-new--read-model "godel-new" (prompt initial history))
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide" (buffer))
(declare-function +workspace-switch "autoload/workspaces" (name &optional auto-create-p))
(declare-function +workspace-exists-p "autoload/workspaces" (name))
(declare-function +workspace-kill "autoload/workspaces" (workspace &optional inhibit-kill-p))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(defconst godel-board-buffer-name "*godel*")

(defconst godel-board-groups
  '(("Needs attention" needs-attention model-mismatch)
    ("Working" working)
    ("Setting up" setting-up)
    ("Needs review" needs-review)
    ("Idle" idle)
    ("Stopped" stopped))
  "Board groups in display order: (LABEL STATE...).")

(defconst godel-board-icons
  '((needs-attention . "●") (model-mismatch . "✗") (working . "◐")
    (setting-up . "…") (needs-review . "✓") (idle . "○") (stopped . "■")))

(defface godel-board-attention '((t :inherit error)) "Tasks needing you.")
(defface godel-board-working '((t :inherit warning)) "Working tasks.")
(defface godel-board-review '((t :inherit success)) "Tasks ready for review.")
(defface godel-board-dim '((t :inherit shadow)) "Secondary text.")

(defun godel-board--state-face (state)
  (pcase state
    ((or 'needs-attention 'model-mismatch) 'godel-board-attention)
    ((or 'working 'setting-up) 'godel-board-working)
    ('needs-review 'godel-board-review)
    (_ 'godel-board-dim)))

(defun godel-board--age (time)
  (if (not time) ""
    (let ((s (- (float-time) time)))
      (cond ((< s 60) (format "%ds" s))
            ((< s 3600) (format "%dm" (/ s 60)))
            ((< s 86400) (format "%dh" (/ s 3600)))
            (t (format "%dd" (/ s 86400)))))))

(defun godel-board--model-label (task)
  (let ((m (godel-models-find (plist-get task :model))))
    (if (or (null m) (godel-models-router-p m))
        (or (plist-get m :name) (plist-get task :model))
      (format "%s/%s" (plist-get m :name) (or (plist-get task :effort) "?")))))

(defun godel-board--repo-name (task)
  (file-name-nondirectory (directory-file-name (plist-get task :repo))))

;;;; Mode

(defvar-keymap godel-board-mode-map
  :parent magit-section-mode-map
  "RET" #'godel-board-visit
  "n" #'godel-board-new
  "m" #'godel-board-message
  "R" #'godel-board-restart
  "x" #'godel-board-teardown
  "g r" #'godel-board-refresh
  "q" #'quit-window)

(define-derived-mode godel-board-mode magit-section-mode "godel"
  "The godel board: every task, grouped by state."
  (setq-local revert-buffer-function (lambda (&rest _) (godel-board-refresh)))
  (setq truncate-lines t))

(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) godel-board-mode-map
    (kbd "RET") #'godel-board-visit
    "n" #'godel-board-new
    "m" #'godel-board-message
    "R" #'godel-board-restart
    "x" #'godel-board-teardown
    "gr" #'godel-board-refresh
    "q" #'quit-window))

;;;; Rendering

(defvar-local godel-board--expanded nil "IDs of tasks whose details are shown.")

(defun godel-board--insert-task (task)
  (let* ((id (plist-get task :id))
         (state (plist-get task :state))
         (face (godel-board--state-face state)))
    (magit-insert-section (godel-task id (not (member id godel-board--expanded)))
      (magit-insert-heading
        (format "  %s %-32s %-10s %-14s %-50s %s"
                (propertize (alist-get state godel-board-icons "?") 'face face)
                (propertize (truncate-string-to-width (plist-get task :title) 32 nil nil "…")
                            'face 'magit-section-secondary-heading)
                (propertize (truncate-string-to-width (godel-board--repo-name task) 10 nil nil "…")
                            'face 'godel-board-dim)
                (truncate-string-to-width (godel-board--model-label task) 14 nil nil "…")
                (propertize (truncate-string-to-width (or (plist-get task :last-event) "") 50 nil nil "…")
                            'face face)
                (propertize (godel-board--age (or (plist-get task :updated) (plist-get task :created)))
                            'face 'godel-board-dim)))
      (dolist (row `(("id" ,id)
                     ("branch" ,(format "%s (from %s)" (plist-get task :branch) (plist-get task :base)))
                     ("worktree" ,(abbreviate-file-name (plist-get task :worktree)))
                     ("model" ,(format "%s%s" (godel-board--model-label task)
                                       (if-let* ((a (plist-get task :model-actual)))
                                           (format "  (running %s)" a) "")))
                     ("changes" ,(plist-get task :diffstat))
                     ("last message" ,(plist-get task :last-message))
                     ("session" ,(if (godel-agent-live-p task) "running" "not running"))))
        (when (cadr row)
          (insert (propertize (format "      %-13s" (car row)) 'face 'godel-board-dim)
                  (cadr row) "\n"))))))

(defun godel-board--render ()
  (let ((inhibit-read-only t)
        (tasks (godel-task-all)))
    (erase-buffer)
    (magit-insert-section (godel-root)
      (insert (propertize (format "godel · %d task%s" (length tasks) (if (= 1 (length tasks)) "" "s"))
                          'face 'magit-section-heading)
              (propertize "   n new · RET open · m message · R restart · x tear down · gr refresh\n\n"
                          'face 'godel-board-dim))
      (when (null tasks)
        (insert "  No tasks yet.  Press n to create one.\n"))
      (pcase-dolist (`(,label . ,states) godel-board-groups)
        (let ((group (cl-remove-if-not (lambda (task) (memq (plist-get task :state) states))
                                       tasks)))
          (when group
            (magit-insert-section (godel-group (car states))
              (magit-insert-heading (format "%s (%d)" label (length group)))
              (mapc #'godel-board--insert-task group)
              (insert "\n"))))))
    ;; magit-insert-section only records HIDE; apply it (magit's own buffers
    ;; do this during their refresh).
    (magit-map-sections (lambda (s) (when (oref s hidden) (magit-section-hide s))))))

(defun godel-board--task-id-at-point ()
  (magit-section-value-if 'godel-task))

(defun godel-board--remember-expanded ()
  (let (ids)
    (when magit-root-section
      (magit-map-sections
       (lambda (s)
         (when (and (eq (oref s type) 'godel-task) (not (oref s hidden)))
           (push (oref s value) ids)))))
    (setq godel-board--expanded ids)))

(defun godel-board-refresh ()
  "Redraw the board, keeping point on the same task."
  (interactive)
  (when-let* ((buffer (get-buffer godel-board-buffer-name)))
    (with-current-buffer buffer
      (let ((id (godel-board--task-id-at-point))
            (line (line-number-at-pos))
            (windows (get-buffer-window-list buffer nil t)))
        (godel-board--remember-expanded)
        (godel-board--render)
        (goto-char (point-min))
        (or (and id (cl-block found
                      (magit-map-sections
                       (lambda (s)
                         (when (and (eq (oref s type) 'godel-task) (equal (oref s value) id))
                           (goto-char (oref s start))
                           (cl-return-from found t))))))
            (forward-line (1- line)))
        (dolist (w windows) (set-window-point w (point)))))))

(defvar godel-board--timer nil)

(defun godel-board--schedule (&rest _)
  "Redraw the board soon (debounced), if it exists."
  (when (and (get-buffer godel-board-buffer-name) (not godel-board--timer))
    (setq godel-board--timer
          (run-with-timer 0.2 nil (lambda ()
                                    (setq godel-board--timer nil)
                                    (godel-board-refresh))))))

;;;###autoload
(defun godel-board ()
  "Show the godel board."
  (interactive)
  (godel-task-load-all)
  (let ((buffer (get-buffer-create godel-board-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'godel-board-mode) (godel-board-mode))
      (godel-board-refresh))
    (switch-to-buffer buffer)))

;;;; Commands

(defun godel-board--task ()
  (or (godel-task-get (godel-board--task-id-at-point))
      (user-error "godel: no task at point")))

(defun godel-board-new ()
  "Create a task in the repo of the task at point, or ask."
  (interactive)
  (godel-new-task (when-let* ((id (godel-board--task-id-at-point)))
                    (plist-get (godel-task-get id) :repo))))

(defun godel-board-visit ()
  "Open the task's workspace: magit-status on the left, Claude on the right."
  (interactive)
  (let* ((task (godel-board--task))
         (buffer (godel-agent-buffer task)))
    (when (fboundp '+workspace-switch)
      (+workspace-switch (plist-get task :slug) t))
    (delete-other-windows)
    (if (file-directory-p (plist-get task :worktree))
        (magit-status-setup-buffer (plist-get task :worktree))
      (dired (plist-get task :repo)))
    (if (and buffer (fboundp 'claude-code-ide--display-buffer-in-side-window))
        (select-window (claude-code-ide--display-buffer-in-side-window buffer))
      (message "godel: no running session; R on the board restarts it"))))

(defun godel-board-message ()
  "Send a message to the task's agent."
  (interactive)
  (let ((task (godel-board--task)))
    (godel-agent-send task (read-string (format "Message %s: " (plist-get task :id))))))

(defun godel-board-restart ()
  "Restart the task's session, resuming the conversation.
Asks for the model and effort, defaulting to the current ones."
  (interactive)
  (require 'godel-new)
  (let* ((task (godel-board--task))
         (id (plist-get task :id))
         (godel-default-model (plist-get task :model))
         (model (godel-new--read-model "Model: " nil nil))
         (entry (godel-models-find model))
         (effort (unless (godel-models-router-p entry)
                   (completing-read "Effort: " (plist-get entry :efforts) nil t nil nil
                                    (or (plist-get task :effort) godel-default-effort))))
         ;; Resume if the session ever started (it has a transcript);
         ;; reusing the ID for a fresh start would be rejected.
         (resume (plist-get task :transcript)))
    (when (or (not (godel-agent-live-p task))
              (y-or-n-p (format "Stop %s's running session and restart it? " id)))
      (godel-agent-stop task)
      (godel-task-update id :model model :effort effort)
      ;; Let claude-code-ide finish cleaning up the old session first.
      (run-with-timer 0.5 nil (lambda () (godel-agent-start (godel-task-get id) resume))))))

(defun godel-board-teardown ()
  "Stop the session, remove the worktree and forget the task.
The branch is kept."
  (interactive)
  (let* ((task (godel-board--task))
         (id (plist-get task :id)))
    (godel-worktree-check
     task
     (lambda (counts)
       (when (y-or-n-p
              (format "Tear down %s?%s The branch %s is kept. "
                      id
                      (if (> (car counts) 0)
                          (format " %d uncommitted file(s) will be LOST." (car counts))
                        "")
                      (concat (plist-get task :branch)
                              (if (> (cdr counts) 0)
                                  (format " (%d unpushed commit(s))" (cdr counts)) ""))))
         (godel-agent-stop task)
         (when (and (fboundp '+workspace-exists-p) (+workspace-exists-p (plist-get task :slug)))
           (+workspace-kill (plist-get task :slug)))
         (godel-task-update id :state 'setting-up :last-event "tearing down")
         (godel-worktree-remove
          task (lambda (err)
                 (if err
                     (godel-task-update id :state 'stopped :last-event err)
                   (godel-task-delete id)
                   (message "godel: %s torn down; branch %s kept"
                            id (plist-get task :branch))))))))))

(add-hook 'godel-event-functions #'godel-board--schedule)

(provide 'godel-board)
;;; godel-board.el ends here
