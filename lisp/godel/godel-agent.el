;;; godel-agent.el --- Claude sessions for godel tasks -*- lexical-binding: t -*-

;;; Commentary:
;; The agent backend.  The only backend wraps claude-code-ide (ghostel
;; terminal).  Every launch:
;;   1. makes sure the notdiamond proxy is running cleanly (proxy guard),
;;   2. builds the environment and flags with `godel-models-launch-env'
;;      (Not Diamond: pin variables removed; other models: pinned),
;;   3. starts claude-code-ide in the task's worktree with godel's hooks,
;;   4. answers the trust dialog if Claude shows one (asking first),
;;   5. on SessionStart checks the model, then sends the pending prompt.
;; See PLAN.md, "Agent backend" and "Model and effort".

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'godel-task)
(require 'godel-models)
(require 'godel-status)
(require 'godel-tui)

(declare-function claude-code-ide "claude-code-ide" ())
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function server-running-p "server" (&optional name))
(defvar claude-code-ide-cli-path)
(defvar claude-code-ide-cli-extra-flags)
(defvar claude-code-ide-focus-on-open)
(defvar claude-code-ide-buffer-name-function)
(defvar ghostel--pid)
(defvar server-name)
(defvar server-socket-dir)

(defcustom godel-default-permission-mode nil
  "Permission mode for new tasks, or nil for Claude's own default."
  :group 'godel
  :type '(choice (const :tag "Claude's default" nil) string))

(defconst godel-agent--dir (file-name-directory (or load-file-name buffer-file-name)))

(defcustom godel-hook-program (expand-file-name "bin/godel-hook" godel-agent--dir)
  "Script Claude Code hooks call to report events to Emacs."
  :group 'godel
  :type 'file)

(defconst godel-agent-hook-events
  '("SessionStart" "UserPromptSubmit" "PreToolUse" "Notification" "Stop"
    "SessionEnd" "PreModelSwitch")
  "Hook events godel listens to.")

(defcustom godel-agent-start-timeout 60
  "Seconds to wait for a session to start (incl. answering the trust dialog)."
  :group 'godel
  :type 'integer)

;;;; Buffers

(defun godel-agent-buffer-name (directory &optional instance)
  "Name for claude-code-ide's buffer in DIRECTORY (and INSTANCE).
Task worktrees get REPO/SLUG so same-named worktrees in different repos
don't collide; other directories keep claude-code-ide's usual name."
  (let ((task (godel-task-by-directory directory)))
    (format "*claude-code[%s%s]*"
            (if task
                (format "%s/%s" (file-name-nondirectory
                                 (directory-file-name (plist-get task :repo)))
                        (plist-get task :slug))
              (file-name-nondirectory (directory-file-name directory)))
            (if instance (concat ":" instance) ""))))

(defun godel-agent-buffer (task)
  "Return TASK's live session buffer, or nil."
  (let ((buf (get-buffer (godel-agent-buffer-name (plist-get task :worktree)))))
    (and (buffer-live-p buf) buf)))

(defun godel-agent-live-p (task)
  "Non-nil if TASK has a running session."
  (when-let* ((buf (godel-agent-buffer task)))
    (process-live-p (get-buffer-process buf))))

;;;; Launch

(defun godel-agent--server-socket ()
  "Path of this Emacs's server socket, starting the server if needed."
  (require 'server)
  (unless (server-running-p server-name)
    (server-start))
  (expand-file-name server-name server-socket-dir))

(defun godel-agent-hooks-json ()
  "The --settings JSON that routes godel's hook events to Emacs."
  (json-encode
   `((hooks . ,(mapcar (lambda (event)
                         (cons (intern event)
                               (vector `((hooks . [((type . "command")
                                                    (command . ,(format "%s %s"
                                                                        (shell-quote-argument godel-hook-program)
                                                                        event)))])))))
                       godel-agent-hook-events)))))

(defun godel-agent-flags (task launch &optional resume)
  "CLI flags (one string) for TASK with LAUNCH from `godel-models-launch-env'.
With RESUME, resume the task's session instead of starting it."
  (mapconcat #'shell-quote-argument
             (append (if resume
                         (list "--resume" (plist-get task :session-id))
                       (list "--session-id" (plist-get task :session-id)))
                     (list "--settings" (godel-agent-hooks-json))
                     (plist-get launch :args)
                     (when-let* ((mode (plist-get task :permission-mode)))
                       (list "--permission-mode" mode)))
             " "))

(defvar godel-agent--starting (make-hash-table :test #'equal)
  "Task ID -> ON-LAUNCH callbacks, while that task's session is starting.")

(defun godel-agent-starting-p (task)
  "Non-nil while TASK's session is being started."
  (gethash (plist-get task :id) godel-agent--starting))

(defun godel-agent--launched (id buffer)
  "Finish starting task ID: call every waiting ON-LAUNCH with BUFFER."
  (let ((callbacks (gethash id godel-agent--starting)))
    (remhash id godel-agent--starting)
    (dolist (cb (reverse callbacks))
      (ignore-errors (funcall cb buffer)))))

(defun godel-agent-start (task &optional resume on-launch)
  "Start (or with RESUME, resume) TASK's Claude session.
The prompt in the task's :pending-prompt is sent once the session has
started on the right model.  ON-LAUNCH, if non-nil, is called with the
session buffer once the launch has returned (nil if it failed); that's
the safe moment to show the buffer.

Only one start runs per task: if one is already in progress, ON-LAUNCH
just waits for it.  Another start can be triggered while one is pending
\(e.g. a workspace switch and RET both resuming), and two launches in one
directory would make claude-code-ide ask for an instance name."
  (let* ((id (plist-get task :id))
         (task (or (godel-task-get id) task))
         (on-launch (or on-launch #'ignore)))
    (if (godel-agent-starting-p task)
        (push on-launch (gethash id godel-agent--starting))
      (godel-agent--start task resume on-launch))))

(defun godel-agent--start (task resume on-launch)
  (let ((id (plist-get task :id)))
    (when (and (godel-models-pin-broken-p)
               (not (godel-models-router-p (godel-models-find (plist-get task :model)))))
      (godel-task-update id :state 'stopped
                         :last-event (format "not started: the model pin isn't working (%s)"
                                             (cdr godel-models-pin-status)))
      (funcall on-launch nil)
      (user-error "godel: pinned models are unavailable: %s" (cdr godel-models-pin-status)))
    (puthash id (list on-launch) godel-agent--starting)
    ;; A resumed session comes back in the state it was in (e.g. still
    ;; ready for review); see the SessionStart case in godel-status.el.
    (when resume
      (godel-task-update id :resume-state (if (memq (plist-get task :state) '(needs-review idle))
                                               (plist-get task :state)
                                             'idle)))
    (godel-task-update id :state 'setting-up
                       :last-event (if resume "resuming the session" "checking the notdiamond proxy"))
    (godel-models-ensure-proxy
     (lambda (ok &optional err)
       (if (not ok)
           (progn (godel-task-update id :state 'stopped :last-event (or err "proxy check failed"))
                  (godel-agent--launched id nil))
         (condition-case e
             (let ((task (godel-task-get id)))
               ;; Last guard: never a second session in the same worktree.
               (unless (godel-agent-live-p task)
                 (godel-agent--launch task resume))
               (godel-agent--launched id (godel-agent-buffer (godel-task-get id))))
           (error (godel-task-update id :state 'stopped
                                     :last-event (format "launch failed: %s"
                                                         (error-message-string e)))
                  (godel-agent--launched id nil))))))))

(defun godel-agent--launch (task resume)
  (require 'claude-code-ide)
  (let* ((id (plist-get task :id))
         (entry (or (godel-models-find (plist-get task :model))
                    (error "Unknown model %s" (plist-get task :model))))
         (launch (godel-models-launch-env entry (or (plist-get task :effort)
                                                    godel-default-effort)))
         (socket (godel-agent--server-socket))
         (default-directory (plist-get task :worktree))
         (process-environment (cons (concat "GODEL_SERVER=" socket)
                                    (plist-get launch :env)))
         (claude-code-ide-cli-path godel-claude-program)
         (claude-code-ide-cli-extra-flags (godel-agent-flags task launch resume))
         (claude-code-ide-focus-on-open nil))
    (godel-task-update id :expect-model (plist-get launch :expect)
                       :pinned (plist-get launch :pinned)
                       :model-actual nil
                       :last-event (format "starting Claude on %s" (plist-get entry :name)))
    (save-window-excursion (claude-code-ide))
    (godel-agent--watch-trust id)))

(defun godel-agent--started-p (id)
  "Non-nil once task ID's session has reported SessionStart."
  (plist-get (godel-task-get id) :model-actual))

(defun godel-agent--watch-trust (id)
  "Until task ID's session starts, answer the trust dialog if it appears."
  (let* ((task (godel-task-get id))
         (buffer (godel-agent-buffer task)))
    (godel-tui-wait
     buffer
     (lambda (text) (or (godel-agent--started-p id) (godel-tui-trust-dialog-p text)))
     godel-agent-start-timeout
     (lambda (text)
       (cond
        ((godel-agent--started-p id))
        ((null text)
         (when (godel-task-get id)
           (godel-task-update id :state 'stopped :last-event "session didn't start")))
        ((y-or-n-p (format "godel: Claude asks whether to trust %s.  Trust it? "
                           (abbreviate-file-name (plist-get task :repo))))
         (godel-agent--answer-trust id buffer))
        (t
         (with-current-buffer buffer (ghostel-send-key "escape"))
         (godel-task-update id :state 'stopped :last-event "repo not trusted")))))))

(defun godel-agent--answer-trust (id buffer)
  "Move the trust dialog's cursor to \"Yes\" in BUFFER, check it, confirm."
  (with-current-buffer buffer (ghostel-send-key "down"))
  (godel-tui-wait
   buffer #'godel-tui-trust-cursor-on-yes-p 3
   (lambda (text)
     (if text
         (with-current-buffer buffer (ghostel-send-key "return"))
       (godel-task-update id :state 'stopped
                          :last-event "couldn't answer the trust dialog; answer it in the session")))))

(defun godel-agent-check-launch (procs entry)
  "Check a session's processes against model ENTRY.
PROCS is a list of (ARGS . ENV) for the session process and its
descendants: ARGS and ENV are lists of strings, as in /proc/PID/cmdline
and /proc/PID/environ.  Returns (OK . DETAIL).
- Not Diamond: no process may carry a pin variable, and Claude must
  have been given --model notdiamond....
- Pinned: Claude must have been given --model with the entry's value."
  (let* ((pins (delete-dups
                (cl-loop for (_ . env) in procs
                         append (cl-loop for e in env
                                         when (cl-some (lambda (v) (string-prefix-p (concat v "=") e))
                                                       godel-pin-variables)
                                         collect e))))
         (model (cl-loop for (args . _) in procs
                         for tail = (member "--model" args)
                         when (cadr tail) return (cadr tail))))
    (cond
     ((null model) (cons nil "couldn't find Claude's --model"))
     ((godel-models-router-p entry)
      (cond (pins (cons nil (format "pin leaked into a Not Diamond session: %s"
                                    (string-join pins ", "))))
            ((not (string-prefix-p "notdiamond" model))
             (cons nil (format "Claude was started with --model %s" model)))
            (t (cons t model))))
     ((equal model (plist-get entry :value)) (cons t model))
     (t (cons nil (format "Claude was started with --model %s, expected %s"
                          model (plist-get entry :value)))))))

(defun godel-agent--proc-file (pid name)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when (ignore-errors (insert-file-contents-literally (format "/proc/%d/%s" pid name)) t)
      (split-string (decode-coding-string (buffer-string) 'utf-8) "\0" t))))

(defun godel-agent--session-procs (task)
  "(ARGS . ENV) for TASK's session process and all its descendants."
  (when-let* ((buffer (godel-agent-buffer task))
              (proc (get-buffer-process buffer))
              ;; ghostel's native terminal gives Emacs a pipe without a PID;
              ;; it records the child's PID itself.
              (root (or (process-id proc)
                        (process-get proc 'ghostel--native-pid)
                        (buffer-local-value 'ghostel--pid buffer))))
    (let* ((all (mapcar (lambda (pid) (cons pid (alist-get 'ppid (process-attributes pid))))
                        (list-system-processes)))
           (tree (list root)) (queue (list root)))
      (while queue
        (let ((parent (pop queue)))
          (dolist (p all)
            (when (and (eql (cdr p) parent) (not (memq (car p) tree)))
              (push (car p) tree) (push (car p) queue)))))
      (mapcar (lambda (pid) (cons (godel-agent--proc-file pid "cmdline")
                                  (godel-agent--proc-file pid "environ")))
              tree))))

(defun godel-agent--on-session-start (task ok)
  "SessionStart handler: stop on a model mismatch, else send the prompt.
Resumed sessions don't report a model, so their processes are checked."
  (let ((id (plist-get task :id)))
    (when (and ok (plist-get task :model-check-pending))
      (pcase-let ((`(,good . ,detail)
                   (godel-agent-check-launch (godel-agent--session-procs task)
                                             (godel-models-find (plist-get task :model)))))
        (setq task (godel-task-update id :model-check-pending nil
                                      :model-actual (if good detail (plist-get task :model-actual))
                                      :state (if good (plist-get task :state) 'model-mismatch)
                                      :last-event (if good (format "session resumed on %s" detail)
                                                    detail))
              ok good)))
    (cond
     ((not ok)
      (message "godel: %s %s; stopping it" id (plist-get task :last-event))
      (godel-agent-stop task))
     ((plist-get task :pending-prompt)
      (godel-tui-wait
       (godel-agent-buffer task) #'godel-tui-input-ready-p 15
       (lambda (text)
         (if (not text)
             (godel-task-update id :state 'needs-attention
                                :last-event "couldn't send the prompt; it's saved on the task")
           (godel-agent-send (godel-task-get id) (plist-get task :pending-prompt))
           (godel-task-update id :pending-prompt nil))))))))

(add-hook 'godel-status-session-start-functions #'godel-agent--on-session-start)

;;;; Resuming

(defun godel-agent-resumable-p (task)
  "Non-nil if TASK's session isn't running but can be resumed.
It must have started before (it has a transcript), its worktree must
exist, and it mustn't be starting already or flagged as a model mismatch
\(which needs a deliberate restart).  Uses the task's current data, not
the (possibly stale) plist passed in."
  (setq task (or (godel-task-get (plist-get task :id)) task))
  (and (not (godel-agent-starting-p task))
       (plist-get task :transcript)
       (plist-get task :session-id)
       (not (memq (plist-get task :state) '(setting-up model-mismatch)))
       (file-directory-p (plist-get task :worktree))
       (not (godel-agent-live-p task))))

(defun godel-agent-ensure-session (task callback)
  "Make sure TASK has a running session, then call CALLBACK with its buffer.
A running session is passed straight away.  A session that can be
resumed (`godel-agent-resumable-p') is resumed, and CALLBACK runs once
the launch has returned.  Otherwise, or if resuming fails, CALLBACK gets
nil."
  (setq task (or (godel-task-get (plist-get task :id)) task))
  (cond
   ((godel-agent-live-p task) (funcall callback (godel-agent-buffer task)))
   ;; Already starting (e.g. from a workspace switch): wait for that start.
   ((godel-agent-starting-p task) (godel-agent-start task t callback))
   ((not (godel-agent-resumable-p task)) (funcall callback nil))
   (t (condition-case err
          (godel-agent-start task t callback)
        (user-error (message "%s" (error-message-string err))
                    (funcall callback nil))))))

;;;; Talking to a session

(defun godel-agent-send (task text)
  "Send TEXT to TASK's session as a prompt."
  (let ((buffer (or (godel-agent-buffer task)
                    (user-error "godel: %s has no running session" (plist-get task :id)))))
    (with-current-buffer buffer (ghostel-paste-string text))
    ;; Give the TUI a moment to take the paste before submitting it.
    (run-with-timer 0.2 nil (lambda ()
                              (when (buffer-live-p buffer)
                                (with-current-buffer buffer (ghostel-send-key "return")))))))

(defun godel-agent-stop (task)
  "Stop TASK's session (kills its terminal buffer)."
  (when-let* ((buffer (godel-agent-buffer task)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(provide 'godel-agent)
;;; godel-agent.el ends here
