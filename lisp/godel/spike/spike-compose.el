;;; spike-compose.el --- does :wq / :q! reach remapped commands? -*- lexical-binding: t -*-
(defvar godel-spike-compose-log nil)
(defun godel-spike-compose-finish () (interactive) (push (list 'finish (buffer-name)) godel-spike-compose-log))
(defun godel-spike-compose-cancel () (interactive) (push (list 'cancel (buffer-name)) godel-spike-compose-log))
(defvar-keymap godel-spike-compose-mode-map
  "C-c C-c" #'godel-spike-compose-finish
  "<remap> <evil-save-and-close>" #'godel-spike-compose-finish
  "<remap> <evil-save-modified-and-close>" #'godel-spike-compose-finish
  "C-c C-k" #'godel-spike-compose-cancel
  "<remap> <evil-quit>" #'godel-spike-compose-cancel)
(define-minor-mode godel-spike-compose-mode "Spike." :keymap godel-spike-compose-mode-map)
(defun godel-spike-compose-try (cmd)
  "Run ex CMD in a fresh compose buffer shown in the main window; return the log."
  (setq godel-spike-compose-log nil)
  (let ((buf (get-buffer-create "*godel-compose-test*")))
    (with-current-buffer buf (erase-buffer) (insert "title\n\nprompt") (godel-spike-compose-mode 1))
    (with-selected-window (window-main-window)
      (switch-to-buffer buf)
      (condition-case err (evil-ex-execute cmd) (error (push (list 'error (error-message-string err)) godel-spike-compose-log))))
    (list cmd godel-spike-compose-log (buffer-live-p buf) (length (window-list)))))
