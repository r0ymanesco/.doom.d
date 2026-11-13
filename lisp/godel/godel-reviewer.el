;;; godel-reviewer.el --- Independent review of a task's changes -*- lexical-binding: t -*-

;;; Commentary:
;; Reviewers that aren't the author (PLAN.md, "Agent reviewers" and the
;; Phase 2 decisions).  Neither can change the task: no godel hooks (so the
;; task's state isn't touched), and no editing tools.
;;
;; Structured review (`godel-reviewer-run', v r): a headless Claude run
;; (`-p', no session saved) gets the task's prompt and full diff, and
;; returns findings, each tied to file and lines.  Its output is streamed
;; (`--output-format stream-json'): each tool call becomes the task's
;; :review-activity, which the board shows next to an elapsed time, and a
;; notification says when it's done.  They're stored on the
;; task as :findings and shown in the review list, where `a' accepts one (it
;; becomes a pending comment) and `x' drops it.
;;
;; Walkthrough (`godel-reviewer-walkthrough', v w): an interactive Claude
;; session in the task's worktree, a claude-code-ide instance named
;; "review", that walks you through the diff and opens each location in
;; Emacs.  It runs read-only (`--permission-mode plan').
;;
;; Both default to the Not Diamond router; with a prefix argument you pick
;; the model.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'godel-task)
(require 'godel-models)
(require 'godel-agent)
(require 'godel-tui)
(require 'godel-review)
(require 'godel-status)

(declare-function claude-code-ide "claude-code-ide" ())
(declare-function godel-new--read-model "godel-new" (prompt initial history))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(defvar claude-code-ide-cli-path)
(defvar claude-code-ide-cli-extra-flags)
(defvar claude-code-ide-focus-on-open)

(defcustom godel-reviewer-model "notdiamond[1m]"
  "Model value for reviewers (see `godel-models')."
  :group 'godel
  :type 'string)

(defcustom godel-reviewer-max-diff 200000
  "Largest diff (in characters) put into a reviewer's prompt.
Beyond this the diff is cut, and the reviewer is told to read the files."
  :group 'godel
  :type 'integer)

(defconst godel-reviewer--read-only-flags
  '("--disallowedTools" "Edit,Write,MultiEdit,NotebookEdit,Bash")
  "Flags that keep a headless reviewer from changing anything.")

;;;; Diff and prompts

(defun godel-reviewer-diff-text (task)
  "TASK's full diff against its merge base, untracked files included."
  (let* ((default-directory (plist-get task :worktree))
         (base (or (godel-review-merge-base task)
                   (user-error "godel: no merge base with %s" (plist-get task :base))))
         (process-environment (cons (concat "GIT_INDEX_FILE=" (godel-review--rebuild-index task))
                                    process-environment)))
    (with-temp-buffer
      (process-file "git" nil t nil "diff" "--no-ext-diff" base)
      (let ((text (buffer-string)))
        (if (> (length text) godel-reviewer-max-diff)
            (concat (substring text 0 godel-reviewer-max-diff)
                    "\n[diff cut here; read the changed files for the rest]\n")
          text)))))

(defun godel-reviewer-prompt (task)
  "The structured review prompt for TASK."
  (format "You are reviewing another engineer's change. Don't change any files.

The task they were given:
%s

Their changes (working tree against %s, new files included):
```diff
%s```

Review the change for bugs, missing cases, unclear code, and anything that
doesn't match the task. Read files for context if you need to. Report only
real problems, most important first; it's fine to report none.

End your reply with the findings as a JSON array between <findings> and
</findings>, each item:
  {\"file\": path relative to the repo root, \"line\": first line in the new
   file, \"end_line\": last line, \"severity\": \"high\" | \"medium\" | \"low\",
   \"comment\": what's wrong and what to do}
"
          (or (plist-get task :prompt) (plist-get task :title))
          (plist-get task :base)
          (godel-reviewer-diff-text task)))

(defun godel-reviewer--json (line)
  (and (string-prefix-p "{" line)
       (ignore-errors (json-parse-string line :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil))))

(defun godel-reviewer-parse (output)
  "Findings from a headless run's OUTPUT: its `result' JSON line (other
lines, from the wrapper or streamed events, are skipped).  Returns a list
of plists (:file :start :end :severity :text), or signals an error with
the reason."
  (let* ((msg (cl-loop for line in (split-string output "\n" t)
                       for m = (godel-reviewer--json line)
                       when (equal (alist-get 'type m) "result") return m))
         (result (alist-get 'result msg)))
    (cond
     ((null msg) (error "No result from the reviewer"))
     ((alist-get 'is_error msg) (error "The reviewer failed: %s" (or result "no details")))
     ((not (and result (string-match "<findings>\\(\\(?:.\\|\n\\)*?\\)</findings>" result)))
      (error "The reviewer's reply had no <findings> block")))
    (mapcar (lambda (f)
              (let ((start (or (alist-get 'line f) 1)))
                (list :file (alist-get 'file f)
                      :start start
                      :end (max start (or (alist-get 'end_line f) start))
                      :severity (or (alist-get 'severity f) "medium")
                      :text (alist-get 'comment f))))
            (json-parse-string (match-string 1 result) :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil))))

;;;; Structured review

(defun godel-reviewer--env (launch)
  "LAUNCH's environment without GODEL_SERVER: a reviewer never reports as
the task, even if Emacs itself was started with that variable."
  (cons "GODEL_SERVER" (plist-get launch :env)))

(defun godel-reviewer--model-entry (model)
  (or (godel-models-find model) (godel-models-find "notdiamond[1m]")
      (car (godel-models))))

(defun godel-reviewer--read-model ()
  (require 'godel-new)
  (let ((godel-default-model godel-reviewer-model))
    (godel-new--read-model "Reviewer model: " nil nil)))

(defun godel-reviewer--stream (program args input env on-message on-exit)
  "Run PROGRAM with ARGS; INPUT on stdin, ENV as its environment.
ON-MESSAGE gets each JSON line (an alist) as it arrives; ON-EXIT gets the
exit status and all the output once the process ends."
  (let* ((buf (generate-new-buffer " *godel-reviewer*"))
         (pending "")
         (process-environment env)
         (proc (make-process
                :name "godel-reviewer" :buffer buf :noquery t
                :command (cons program args) :connection-type 'pipe
                :stderr (get-buffer-create " *godel-reviewer-stderr*")
                :filter (lambda (p out)
                          (when (buffer-live-p (process-buffer p))
                            (with-current-buffer (process-buffer p)
                              (goto-char (point-max)) (insert out)))
                          (let ((lines (split-string (concat pending out) "\n")))
                            (setq pending (car (last lines)))
                            (dolist (line (butlast lines))
                              (when-let* ((msg (godel-reviewer--json line)))
                                (ignore-errors (funcall on-message msg))))))
                :sentinel (lambda (p _)
                            (unless (process-live-p p)
                              (let ((out (with-current-buffer (process-buffer p) (buffer-string))))
                                (kill-buffer (process-buffer p))
                                (funcall on-exit (process-exit-status p) out)))))))
    (process-send-string proc input)
    (process-send-eof proc)
    proc))

(defun godel-reviewer--activity (msg)
  "A short description of the tool call in stream message MSG, or nil."
  (when (equal (alist-get 'type msg) "assistant")
    (cl-loop for part in (alist-get 'content (alist-get 'message msg))
             when (equal (alist-get 'type part) "tool_use")
             return (godel-status-tool-summary
                     (alist-get 'name part)
                     (let ((input (alist-get 'input part)))
                       ;; Paths relative to the worktree read better.
                       (if-let* ((f (alist-get 'file_path input)))
                           (cons (cons 'file_path (file-name-nondirectory f)) input)
                         input))))))

(defun godel-reviewer-run (task &optional model)
  "Start a structured review of TASK on MODEL (default `godel-reviewer-model')."
  (let* ((id (plist-get task :id))
         (entry (godel-reviewer--model-entry (or model godel-reviewer-model)))
         (launch (godel-models-launch-env entry godel-default-effort))
         (prompt (godel-reviewer-prompt task)))
    (when (eq (plist-get task :review-status) 'running)
      (user-error "godel: a review of %s is already running" id))
    (godel-task-update id :review-status 'running :review-started (float-time)
                       :review-activity "starting" :review-seen nil
                       :review-note (format "reviewing on %s" (plist-get entry :name)))
    (godel-reviewer--start-ticking)
    (message "godel: reviewing %s on %s; the board shows its progress" id (plist-get entry :name))
    (godel-models-ensure-proxy
     (lambda (ok &optional err)
       (if (not ok)
           (godel-reviewer--done id nil (or err "proxy check failed"))
         (let ((default-directory (plist-get task :worktree)))
           (godel-reviewer--stream
            godel-claude-program
            (append '("-p" "--output-format" "stream-json" "--verbose" "--no-session-persistence")
                    godel-reviewer--read-only-flags
                    (plist-get launch :args))
            prompt
            (godel-reviewer--env launch)
            (lambda (msg)
              (when-let* ((activity (godel-reviewer--activity msg))
                          (task (godel-task-get id)))
                (unless (equal activity (plist-get task :review-activity))
                  (godel-task-update id :review-activity activity))))
            (lambda (_exit output)
              (condition-case err
                  (godel-reviewer--done id (godel-reviewer-parse output) nil)
                (error (godel-reviewer--done id nil (error-message-string err))))))))))))

;; While a review runs, redraw the board every second for its elapsed time.
(declare-function godel-board--schedule "godel-board" (&rest _))
(defvar godel-reviewer--tick-timer nil)

(defun godel-reviewer--start-ticking ()
  (unless godel-reviewer--tick-timer
    (setq godel-reviewer--tick-timer (run-with-timer 1 1 #'godel-reviewer--tick))))

(defun godel-reviewer--tick ()
  (if (cl-some (lambda (task) (eq (plist-get task :review-status) 'running)) (godel-task-all))
      (when (fboundp 'godel-board--schedule) (godel-board--schedule))
    (cancel-timer godel-reviewer--tick-timer)
    (setq godel-reviewer--tick-timer nil)))

(defun godel-reviewer--done (id findings error)
  "Store a finished review of task ID: FINDINGS, or ERROR (a string)."
  (when-let* ((task (godel-task-get id)))
    (if error
        (progn (godel-task-update id :review-status 'failed :review-note error :review-activity nil)
               (godel-notify (format "godel: review of %s failed" (plist-get task :title)) error))
      (let* ((seq (or (plist-get task :finding-seq) 0))
             (new (mapcar (lambda (f)
                            (append (list :id (cl-incf seq) :state 'proposed :created (float-time)) f))
                          findings)))
        (godel-task-update id :finding-seq seq
                           :findings (append (cl-remove 'proposed (plist-get task :findings)
                                                        :key (lambda (f) (plist-get f :state)))
                                             new)
                           :review-status 'done :review-activity nil
                           :review-note (format "%d finding%s" (length new) (if (= 1 (length new)) "" "s")))
        (godel-notify (format "godel: review of %s is done" (plist-get task :title))
                      (if new
                          (format "%d finding%s (v l on the board to see them)"
                                  (length new) (if (= 1 (length new)) "" "s"))
                        "No findings"))))))

(defun godel-reviewer-accept (id finding-id)
  "Turn finding FINDING-ID of task ID into a pending comment."
  (let* ((task (godel-task-get id))
         (f (cl-find finding-id (plist-get task :findings) :key (lambda (f) (plist-get f :id)))))
    (godel-review-add-comment id (plist-get f :file) (plist-get f :start) (plist-get f :end)
                              (format "[reviewer, %s] %s" (plist-get f :severity) (plist-get f :text)))
    (godel-reviewer--set-finding-state id finding-id 'accepted)))

(defun godel-reviewer-drop (id finding-id)
  "Drop finding FINDING-ID of task ID."
  (godel-reviewer--set-finding-state id finding-id 'dropped))

(defun godel-reviewer--set-finding-state (id finding-id state)
  (godel-task-update
   id :findings (mapcar (lambda (f)
                          (if (equal (plist-get f :id) finding-id)
                              (plist-put (copy-sequence f) :state state)
                            f))
                        (plist-get (godel-task-get id) :findings))))

;;;; Walkthrough

(defconst godel-reviewer-walkthrough-instance "review")

(defun godel-reviewer-walkthrough-prompt (task)
  (format "Walk me through the changes in this worktree, one hunk at a time.
They're against %s, new files included:
```diff
%s```
For each hunk: open the file at that spot with the openFile tool, explain
what changed and why it matters, point out anything questionable, then stop
and wait for me before the next hunk. Don't change any files."
          (plist-get task :base) (godel-reviewer-diff-text task)))

(defun godel-reviewer-walkthrough (task &optional model)
  "Start an interactive, read-only walkthrough of TASK's changes."
  (require 'claude-code-ide)
  (let* ((entry (godel-reviewer--model-entry (or model godel-reviewer-model)))
         (launch (godel-models-launch-env entry godel-default-effort))
         (prompt (godel-reviewer-walkthrough-prompt task)))
    (godel-models-ensure-proxy
     (lambda (ok &optional err)
       (if (not ok)
           (message "godel: walkthrough not started: %s" (or err "proxy check failed"))
         (let* ((default-directory (plist-get task :worktree))
                ;; No GODEL_SERVER and no godel hooks: this session must not
                ;; report as the task.
                (process-environment (godel-reviewer--env launch))
                (claude-code-ide-cli-path godel-claude-program)
                (claude-code-ide-cli-extra-flags
                 (mapconcat #'shell-quote-argument
                            (append '("--permission-mode" "plan") (plist-get launch :args))
                            " "))
                (claude-code-ide-focus-on-open t)
                ;; claude-code-ide asks for an instance name when it's offered
                ;; one; always offer it, and answer it ourselves.
                (current-prefix-arg '(4)))
           (cl-letf (((symbol-function 'claude-code-ide--read-instance-name)
                      (lambda (&rest _) godel-reviewer-walkthrough-instance)))
             (claude-code-ide))
           (let ((buffer (get-buffer (godel-agent-buffer-name
                                      (plist-get task :worktree)
                                      godel-reviewer-walkthrough-instance))))
             (if (not buffer)
                 (message "godel: the walkthrough session didn't start")
               (godel-tui-wait buffer #'godel-tui-input-ready-p 30
                               (lambda (screen)
                                 (if screen
                                     (with-current-buffer buffer
                                       (ghostel-paste-string prompt)
                                       (run-with-timer 0.2 nil (lambda ()
                                                                 (when (buffer-live-p buffer)
                                                                   (with-current-buffer buffer
                                                                     (ghostel-send-key "return"))))))
                                   (message "godel: the walkthrough session isn't ready; type your request there"))))))))))))

(provide 'godel-reviewer)
;;; godel-reviewer.el ends here
