;;; godel-status.el --- Hook events -> task states -*- lexical-binding: t -*-

;;; Commentary:
;; Claude Code hooks run bin/godel-hook, which calls
;; (godel-status-event EVENT BASE64-JSON) through emacsclient.  This file
;; turns those events into task states (PLAN.md, "Status from hooks").
;; The task is found by its session ID (godel chooses it at launch), falling
;; back to the working directory.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'godel-task)

(defvar godel-status-session-start-functions nil
  "Functions called when a task's session starts.
Each gets the task and a flag: non-nil if the session reported the
expected model.  godel-agent uses this to send the first prompt or to
stop a session that started on the wrong model.")

(defvar godel-status--seq (make-hash-table :test #'equal)
  "Task ID -> number of events seen, to discard stale async results.")

(defun godel-status--decode (payload)
  (json-parse-string (decode-coding-string (base64-decode-string payload) 'utf-8)
                     :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun godel-status--truncate (s n)
  (let ((s (replace-regexp-in-string "[\n\t ]+" " " (or s ""))))
    (if (> (length s) n) (concat (substring s 0 (1- n)) "…") s)))

(defun godel-status-tool-summary (tool input)
  "Short description of TOOL called with INPUT (an alist)."
  (let ((detail (or (alist-get 'command input)
                    (when-let* ((f (alist-get 'file_path input))) (file-name-nondirectory f))
                    (alist-get 'pattern input)
                    (alist-get 'url input)
                    (alist-get 'description input))))
    (if detail
        (format "%s(%s)" tool (godel-status--truncate detail 60))
      tool)))

;;;###autoload
(defun godel-status-event (event payload)
  "Handle hook EVENT (a string) with base64-encoded JSON PAYLOAD.
Called by bin/godel-hook through emacsclient.  Never signals: a failing
hook must not disturb the session."
  (condition-case err
      (let* ((data (godel-status--decode payload))
             (task (or (godel-task-by-session (alist-get 'session_id data))
                       (and (alist-get 'cwd data)
                            (godel-task-by-directory (alist-get 'cwd data))))))
        (when task
          (godel-status-handle (plist-get task :id) event data)))
    (error (message "godel: error handling %s hook: %s" event (error-message-string err))))
  nil)

(defun godel-status-handle (id event data)
  "Apply hook EVENT with DATA (an alist) to task ID."
  (let ((task (godel-task-get id))
        (seq (puthash id (1+ (gethash id godel-status--seq 0)) godel-status--seq)))
    (pcase event
      ("SessionStart"
       ;; Resumed sessions don't report a model; godel-agent then checks the
       ;; running processes instead (`godel-agent-check-launch').
       (let* ((model (alist-get 'model data))
              (expect (plist-get task :expect-model))
              (ok (or (null expect) (null model) (godel-status-same-model-p model expect))))
         (setq task (godel-task-update
                     id :session-id (or (alist-get 'session_id data) (plist-get task :session-id))
                     :transcript (alist-get 'transcript_path data)
                     :model-actual (or model "unreported")
                     :model-check-pending (and expect (null model))
                     :state (if ok (if (plist-get task :pending-prompt) 'setting-up 'idle)
                              'model-mismatch)
                     :last-event (cond ((not ok) (format "started on %s, expected %s" model expect))
                                       (model (format "session started on %s" model))
                                       (t "session resumed; checking its model"))))
         (run-hook-with-args 'godel-status-session-start-functions task ok)))
      ("UserPromptSubmit"
       (godel-task-update id :state 'working
                          :last-event (concat "prompt: " (godel-status--truncate
                                                          (alist-get 'prompt data) 60))))
      ("PreToolUse"
       (godel-task-update id :state 'working
                          :last-tool (godel-status-tool-summary (alist-get 'tool_name data)
                                                                (alist-get 'tool_input data))
                          :last-event (godel-status-tool-summary (alist-get 'tool_name data)
                                                                 (alist-get 'tool_input data))))
      ("Notification"
       (let ((type (alist-get 'notification_type data)))
         ;; Claude's "waiting for your input" reminder arrives a minute after a
         ;; finished turn; the board already shows the task as idle or ready
         ;; for review, so it's not a request for attention.
         (unless (or (member type '("idle_prompt" "auth_success"))
                     (string-match-p "waiting for your input" (or (alist-get 'message data) "")))
           (godel-task-update
          id :state 'needs-attention
          :last-event (if (and (equal type "permission_prompt") (plist-get task :last-tool))
                          (concat "permission: " (plist-get task :last-tool))
                        (or (alist-get 'message data) "needs your input"))))))
      ("Stop"
       (godel-task-update id :last-message (godel-status--truncate
                                            (alist-get 'last_assistant_message data) 200))
       (unless (eq (plist-get (godel-task-get id) :state) 'model-mismatch)
       (godel-status-check-changes
        id (lambda (changed stat)
             ;; Skip if another event arrived while git was running.
             (when (and (godel-task-get id) (= seq (gethash id godel-status--seq 0)))
               (godel-task-update id :state (if changed 'needs-review 'idle)
                                  :diffstat stat
                                  :last-event (godel-status--truncate
                                               (alist-get 'last_assistant_message data) 80)))))))
      ("SessionEnd"
       (unless (eq (plist-get task :state) 'model-mismatch)
         (godel-task-update id :state 'stopped
                            :last-event (format "session ended (%s)"
                                                (or (alist-get 'reason data) "exit")))))
      ("PreModelSwitch"
       (godel-task-update id :model-actual (alist-get 'to_model data)
                          :last-event (format "model %s -> %s"
                                              (alist-get 'from_model data)
                                              (alist-get 'to_model data)))))))

(defun godel-status--base-model (model)
  "MODEL without a context suffix such as [1m]."
  (replace-regexp-in-string "\\[[^]]*\\]\\'" "" (or model "")))

(defun godel-status-same-model-p (a b)
  "Non-nil if model names A and B are the same, ignoring [1m]-style suffixes."
  (equal (godel-status--base-model a) (godel-status--base-model b)))

(defun godel-status-check-changes (id callback)
  "Asynchronously check task ID's worktree for changes against its base.
Calls CALLBACK with CHANGED (non-nil if there are committed, uncommitted
or untracked changes since the merge base) and a short diffstat string."
  (let* ((task (godel-task-get id))
         (default-directory (plist-get task :worktree))
         (buf (generate-new-buffer " *godel-diffstat*")))
    (if (not (file-directory-p default-directory))
        (funcall callback nil nil)
      (make-process
       :name "godel-diffstat" :buffer buf :noquery t :connection-type 'pipe
       :command (list "sh" "-c"
                      "mb=$(git merge-base \"$1\" HEAD) || exit 2
git diff --shortstat \"$mb\"
echo \"untracked $(git ls-files --others --exclude-standard | wc -l)\""
                      "godel" (plist-get task :base))
       :sentinel
       (lambda (p _)
         (unless (process-live-p p)
           (let* ((out (with-current-buffer buf (buffer-string)))
                  (lines (split-string out "\n" t "[ \t]+"))
                  (untracked (or (cl-some (lambda (l) (and (string-match "^untracked \\([0-9]+\\)" l)
                                                           (string-to-number (match-string 1 l))))
                                          lines)
                                 0))
                  (shortstat (cl-find-if (lambda (l) (string-match-p "changed" l)) lines))
                  (stat (string-join (delq nil (list shortstat
                                                     (and (> untracked 0)
                                                          (format "%d untracked" untracked))))
                                     ", ")))
             (kill-buffer buf)
             (funcall callback (or shortstat (> untracked 0))
                      (unless (string-empty-p stat) stat)))))))))

(provide 'godel-status)
;;; godel-status.el ends here
