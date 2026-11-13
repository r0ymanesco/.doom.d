;;; godel-sidebar.el --- The board in a sidebar -*- lexical-binding: t -*-

;;; Commentary:
;; The compact board (godel-board.el) in a side window on the left.
;;
;; - Opens automatically in every task workspace, however you get there.
;; - `godel-sidebar-toggle' (SPC G o) hides it everywhere until it's toggled
;;   again; in other workspaces it opens it too.
;; - Doom saves a window layout per workspace, so the sidebar is re-attached
;;   (or removed, if hidden) after each workspace switch.
;; See PLAN.md, "Sidebar".

;;; Code:

(require 'godel-task)
(require 'godel-board)

(declare-function +workspace-current-name "autoload/workspaces" ())

(defcustom godel-sidebar-auto-open t
  "Non-nil to open the sidebar automatically in task workspaces."
  :group 'godel
  :type 'boolean)

(defvar godel-sidebar-hidden nil
  "Non-nil after the sidebar was hidden with `godel-sidebar-toggle'.
It then stays hidden in every workspace until toggled again.")

(defun godel-sidebar-window (&optional frame)
  "The window showing the sidebar in FRAME (default: selected), or nil."
  (when-let* ((buffer (get-buffer godel-sidebar-buffer-name)))
    (get-buffer-window buffer frame)))

(defun godel-sidebar-show ()
  "Show the sidebar in the selected frame without selecting it."
  (let ((buffer (godel-board-buffer t)))
    (or (godel-sidebar-window)
        (let ((window (display-buffer-in-side-window
                       buffer
                       `((side . left)
                         ;; Its own slot, so it can share the left with Treemacs.
                         (slot . 1)
                         (window-width . ,godel-sidebar-width)
                         (preserve-size . (t . nil))
                         (window-parameters . ((no-delete-other-windows . t)))))))
          (when window
            (set-window-dedicated-p window t))
          window))))

(defun godel-sidebar-hide ()
  "Hide the sidebar everywhere until `godel-sidebar-toggle' shows it again."
  (interactive)
  (setq godel-sidebar-hidden t)
  (godel-sidebar--remove))

(defun godel-sidebar--remove ()
  (when-let* ((buffer (get-buffer godel-sidebar-buffer-name)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (ignore-errors (delete-window window)))))

;;;###autoload
(defun godel-sidebar-toggle ()
  "Show or hide the sidebar.  Hiding it hides it in every workspace."
  (interactive)
  (if (godel-sidebar-window)
      (godel-sidebar-hide)
    (setq godel-sidebar-hidden nil)
    (godel-sidebar-show)))

(defun godel-sidebar--task-workspace-p ()
  (and (fboundp '+workspace-current-name)
       (godel-task-by-slug (ignore-errors (+workspace-current-name)))))

(defun godel-sidebar-sync (&rest _)
  "Make the sidebar match the user's choice in the current workspace.
Runs after workspace switches (and after `godel-board-visit'): removes it
if hidden, shows it in task workspaces when `godel-sidebar-auto-open',
and redraws it so the current task is marked."
  ;; After the switch has restored the workspace's window layout.
  (run-with-timer
   0 nil
   (lambda ()
     (cond (godel-sidebar-hidden (godel-sidebar--remove))
           ((and godel-sidebar-auto-open (godel-sidebar--task-workspace-p))
            (godel-sidebar-show)))
     (when (get-buffer godel-sidebar-buffer-name)
       (godel-board--refresh-buffer (get-buffer godel-sidebar-buffer-name))))))

(provide 'godel-sidebar)
;;; godel-sidebar.el ends here
