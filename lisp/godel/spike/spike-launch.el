;;; spike-launch.el --- launch via claude-code-ide with per-launch flags -*- lexical-binding: t -*-
(require 'claude-code-ide)
(defvar godel-spike-hook-script "/home/tzeyang/.paradev/worktrees/doom/design-mode-emacs-turns-797c93aa/lisp/godel/spike/godel-hook")
(defun godel-spike-hooks-json ()
  (json-encode
   (list (cons 'hooks
               (mapcar (lambda (e)
                         (cons e (vector (list (cons 'hooks (vector (list (cons 'type "command")
                                                                          (cons 'command (format "%s %s" godel-spike-hook-script e)))))))))
                       '(SessionStart UserPromptSubmit PreToolUse PostToolUse Notification Stop SessionEnd PreModelSwitch))))))
(defun godel-spike-launch (dir sid effort)
  "Start a claude-code-ide session in DIR with per-launch flags."
  (let ((default-directory (file-name-as-directory dir))
        (claude-code-ide-cli-extra-flags
         (format "--session-id %s --effort %s --settings %s"
                 sid effort (shell-quote-argument (godel-spike-hooks-json)))))
    (claude-code-ide)
    (list :global-flags-after (default-value 'claude-code-ide-cli-extra-flags))))
