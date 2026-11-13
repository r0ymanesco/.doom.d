;;; godel.el --- An agent development environment for Emacs -*- lexical-binding: t -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (magit-section "4.0") (transient "0.7") (claude-code-ide "0.2") (markdown-mode "2.6"))
;; Keywords: tools

;;; Commentary:
;; godel turns Emacs into an agent development environment: each task gets
;; a git worktree, a branch and a Claude session; hooks report each
;; session's state to a board.  See PLAN.md.
;;
;; `godel-mode' is the on/off switch.  It starts the Emacs server (hooks
;; report through emacsclient), names Claude buffers per task, marks tasks
;; whose sessions died with a previous Emacs as stopped, sends desktop
;; notifications, and shows a count of tasks needing attention in the mode
;; line.

;;; Code:

(require 'cl-lib)
(require 'godel-task)
(require 'godel-models)
(require 'godel-status)
(require 'godel-agent)
(require 'godel-worktree)
(require 'godel-compose)
(require 'godel-new)
(require 'godel-board)
(require 'godel-sidebar)
(require 'godel-api)
(require 'godel-review)
(require 'godel-reviewer)
(require 'godel-ship)

(declare-function notifications-notify "notifications" (&rest params))
(defvar claude-code-ide-buffer-name-function)
(defvar persp-activated-functions)
(defvar server-name)
(declare-function server-running-p "server" (&optional name))

(defcustom godel-notify-states '(needs-attention needs-review model-mismatch)
  "Task states that trigger a desktop notification."
  :group 'godel
  :type '(repeat symbol))

(defvar godel--saved-buffer-name-function nil)

;;;; Notifications and mode line

(defvar godel--last-states (make-hash-table :test #'equal)
  "Task ID -> last state notified about, so each change notifies once.")

(defun godel--notify (task change)
  (let* ((id (plist-get task :id))
         (state (plist-get task :state))
         (old (gethash id godel--last-states)))
    (if (eq change 'deleted)
        (remhash id godel--last-states)
      (puthash id state godel--last-states)
      (when (and (memq state godel-notify-states) (not (eq state old)))
        (let ((title (format "godel: %s %s" (plist-get task :title)
                             (pcase state
                               ('needs-attention "needs you")
                               ('needs-review "is ready for review")
                               ('model-mismatch "started on the wrong model")
                               (_ (symbol-name state)))))
              (body (or (plist-get task :last-event) "")))
          (if (and (require 'notifications nil t)
                   (ignore-errors (notifications-notify :title title :body body :app-name "godel")))
              nil
            (message "%s: %s" title body)))))
    (force-mode-line-update t)))

(defun godel--attention-count ()
  (cl-count-if (lambda (task) (memq (plist-get task :state) '(needs-attention model-mismatch)))
               (godel-task-all)))

(defun godel--review-count ()
  (cl-count-if (lambda (task) (eq (plist-get task :state) 'needs-review)) (godel-task-all)))

(defvar godel-mode-line
  '(:eval (let ((a (godel--attention-count)) (r (godel--review-count)))
            (when (> (+ a r) 0)
              (concat " "
                      (when (> a 0) (propertize (format "●%d" a) 'face 'godel-state-needs-attention))
                      (when (> r 0) (propertize (format "✓%d" r) 'face 'godel-state-needs-review))
                      " "))))
  "Mode line construct showing tasks needing attention / review.")
(put 'godel-mode-line 'risky-local-variable t)

(defun godel-workspace-badge (name)
  "Badge for the Doom workspace NAME if it belongs to a task, else \"\".
The badge carries its state's face, for the tab bar to apply."
  (if-let* ((task (godel-task-by-slug name))
            (state (plist-get task :state))
            (icon (pcase state
                    ((or 'needs-attention 'model-mismatch) "●")
                    ('needs-review "✓")
                    ('working "◐")
                    ('setting-up "…"))))
      (propertize icon 'face (godel-board--state-face state))
    ""))

;;;; Startup

(defun godel--reconcile ()
  "Mark tasks whose session isn't running as stopped.
Sessions run inside Emacs, so after a restart they're gone; hook events
for them can't arrive.  Reviewable states are kept."
  (dolist (task (godel-task-all))
    (when (and (memq (plist-get task :state) '(working needs-attention setting-up))
               (not (godel-agent-live-p task)))
      (godel-task-update (plist-get task :id) :state 'stopped
                         :last-event "session ended (Emacs restarted?); opening the task resumes it"))))

;;;###autoload
(define-minor-mode godel-mode
  "Agent development environment: tasks, worktrees and Claude sessions."
  :global t
  :lighter nil
  :group 'godel
  (if godel-mode
      (progn
        (require 'server)
        (unless (server-running-p server-name) (server-start))
        (when-let* ((pins (godel-models-inherited-pins)))
          (display-warning 'godel
                           (format "Emacs's environment sets %s.  godel removes them for Not Diamond launches, but other tools launched from Emacs will inherit them."
                                   (string-join pins ", "))))
        (setq godel--saved-buffer-name-function
              (bound-and-true-p claude-code-ide-buffer-name-function))
        (setq claude-code-ide-buffer-name-function #'godel-agent-buffer-name)
        (godel-task-load-all)
        (godel--reconcile)
        (add-hook 'godel-event-functions #'godel--notify)
        ;; Doom's workspace hook isn't a normal hook; config.el uses add-to-list too.
        (when (boundp 'persp-activated-functions)
          (add-to-list 'persp-activated-functions #'godel-sidebar-sync 'append)
          (add-to-list 'persp-activated-functions #'godel-board-on-workspace-activated 'append))
        (add-to-list 'global-mode-string 'godel-mode-line t)
        (godel-ship-start-checking)
        (godel-models-refresh-if-stale))
    (remove-hook 'godel-event-functions #'godel--notify)
    (godel-ship-stop-checking)
    (when (boundp 'persp-activated-functions)
      (setq persp-activated-functions
            (delq #'godel-board-on-workspace-activated
                  (delq #'godel-sidebar-sync persp-activated-functions))))
    (setq global-mode-string (delq 'godel-mode-line global-mode-string))
    (when godel--saved-buffer-name-function
      (setq claude-code-ide-buffer-name-function godel--saved-buffer-name-function))))

(provide 'godel)
;;; godel.el ends here
