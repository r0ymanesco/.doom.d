;;; godel-reviewer.el --- Independent review of a task's changes -*- lexical-binding: t -*-

;;; Commentary:
;; Reviewers that aren't the author (PLAN.md, "Agent reviewers" and the
;; Phase 2 decisions).  Neither can change the task: no godel hooks (so the
;; task's state isn't touched), and no editing tools.
;;
;; Both start by asking the author for a briefing (`godel-reviewer-briefing'):
;; a headless *fork* of the author's session (`--resume ID --fork-session -p',
;; read-only, not saved, so the live session is untouched), which knows the
;; problem, why each choice was made (including what you decided in the
;; conversation) and what was tested, and which a reviewer starting from the
;; diff alone doesn't.  The briefing goes into the reviewer's prompt, as the
;; author's account to check against the code rather than to trust.  A task
;; whose session never started, or `godel-reviewer-ask-author' nil, skips it;
;; if the author can't answer (or `godel-reviewer-briefing-timeout' passes),
;; the review goes ahead without it and says so.  A review and a walkthrough
;; started together share one answer.  A structured review's briefing is
;; saved on the task as :review-briefing and shown, collapsed, in the review
;; list; a walkthrough's isn't kept (it's in its first message).
;;
;; Structured review (`godel-reviewer-run', v r): a headless Claude run
;; (`-p', no session saved) gets the task's prompt, the briefing and the full
;; diff, and returns findings, each tied to file and lines.  Its output is streamed
;; (`--output-format stream-json'): each tool call becomes the task's
;; :review-activity, which the board shows next to an elapsed time, and a
;; notification says when it's done.  They're stored on the
;; task as :findings and shown in the review list, where `a' accepts one (it
;; becomes a pending comment) and `x' drops it.
;;
;; Walkthrough (`godel-reviewer-walkthrough', v w): an interactive Claude
;; session in the task's worktree, a claude-code-ide instance named
;; "review", that walks you through the diff, showing each location in
;; Emacs with godel's `godel_show_location' tool.  It can't change files
;; (editing and shell tools are disallowed; not plan mode, which asks
;; before every MCP tool call).  You talk to it in
;; its window; it records review comments with godel's MCP tools
;; (`godel_add_comment', `godel_list_comments', `godel_show_location' on
;; claude-code-ide's emacs-tools server), both on its own and when you ask
;; or agree.  The tools only work for a task's walkthrough session.
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
(declare-function +workspace-current-name "autoload/workspaces" ())
(defvar claude-code-ide-cli-path)
(defvar claude-code-ide-cli-extra-flags)
(defvar claude-code-ide-focus-on-open)

(defcustom godel-reviewer-model "notdiamond[1m]"
  "Model value for reviewers (see `godel-models')."
  :group 'godel
  :type 'string)

(defcustom godel-reviewer-ask-author t
  "Whether a reviewer first asks the task's author for a briefing.
The author's session knows the problem, why it chose its approach and what
it tested.  It costs one extra turn of the author's model (a fork of its
conversation, which is long for an old task); nil reviews from the task's
prompt and the diff alone."
  :group 'godel
  :type 'boolean)

(defcustom godel-reviewer-prepared-max-age 600
  "Seconds a walkthrough's briefing stays good when it couldn't be used at once.
If you'd moved on when it arrived, godel keeps it and the next `v w' uses it
instead of asking the author again, if it's no older than this."
  :group 'godel
  :type 'integer)

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

(defconst godel-reviewer--briefing-request
  "This is a request from the review tool, not from the user, and it isn't
part of the conversation to be briefed on.  A reviewer is about to review the
changes you made in this task.  It wasn't part of your conversation with the
user and can't see it, so write it a briefing of that conversation.

Write from your own point of view: refer to yourself as \"I\", to the person
you've been working with as \"the user\", and to the reviewer as \"the
reviewer\".  Don't address the reviewer as \"you\", and don't write \"you\" for
the user.  Don't change any files, and don't paste the diff.

Reply with these three parts and nothing else:

## Problem
What the user asked for and why. Include the requirements they added,
constraints they set, and things they corrected or told me not to do.

## Solution
What I changed and why I chose that approach. Include the decisions made in
the conversation (by me or by the user), alternatives that were considered and
rejected and why, trade-offs accepted, and anything deliberately left undone.

## Tested
What I actually ran or checked, and the results. Say what I did not test or
couldn't, and anything I'm unsure about.

Be factual: if something wasn't verified, say so rather than implying it was."
  "What the author is asked for the reviewer's briefing.")

(defun godel-reviewer--briefing-section (briefing)
  "The part of a reviewer's prompt that carries BRIEFING, or \"\" without one."
  (if (string-empty-p (string-trim (or briefing "")))
      ""
    (format "The author's briefing, from the conversation they had with the user
while making the change (you didn't see that conversation). It explains the
problem, the decisions behind the approach, and what they tested. Use it for
context, but don't take it on trust: check it against the code, and point out
where the change doesn't do what it says or where its testing leaves a gap.
<briefing>
%s
</briefing>

" (string-trim briefing))))

(defun godel-reviewer--fresh-diff (task earlier)
  "TASK's diff now; EARLIER, the one taken when the review began, if that fails.
The author can keep working while it's asked for a briefing, which takes up
to `godel-reviewer-briefing-timeout' seconds; a reviewer reads the files as
they are when it runs, so the diff it's given should match them."
  (or (ignore-errors (godel-reviewer-diff-text task)) earlier))

(defun godel-reviewer-prompt (task &optional briefing diff)
  "The structured review prompt for TASK, with the author's BRIEFING if any.
DIFF is the task's diff if already taken (`godel-reviewer-diff-text')."
  (format "You are reviewing another engineer's change. Don't change any files.

The task they were given:
%s

%sTheir changes (working tree against %s, new files included):
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
          (godel-reviewer--briefing-section briefing)
          (plist-get task :base)
          (or diff (godel-reviewer-diff-text task))))

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

;;;; The author's briefing

(defcustom godel-reviewer-briefing-timeout 300
  "Seconds to wait for the author's briefing before reviewing without it."
  :group 'godel
  :type 'integer)

(defvar godel-reviewer--briefing-runs (make-hash-table :test #'equal)
  "Task ID -> (PROCESS . CALLBACKS), while the task's author is being asked.
Only in memory: a briefing doesn't survive a restart, so neither does this.")

(defvar godel-reviewer--asking (make-hash-table :test #'equal)
  "Task ID -> time, while a walkthrough waits for the task's author's briefing.
Kept out of the task store, so a restart can't leave a stale badge.")

(defvar godel-reviewer--walkthrough-starting (make-hash-table :test #'equal)
  "Task ID -> token (START-TIME), while a walkthrough of the task is starting.
From `v w' until its first message.  A second `v w' meanwhile would start a
second session and paste the first message twice.  Only in memory.  A token
that outlives `godel-reviewer--walkthrough-deadline' is ignored, so a step
that never answers (a hung `notdiamond start') can't refuse `v w' for good.")

(defun godel-reviewer--walkthrough-deadline ()
  "Seconds after which a walkthrough that is still starting is given up on:
the proxy check, the briefing, and the session coming up, with slack."
  (+ godel-reviewer-briefing-timeout 180))

(defun godel-reviewer--walkthrough-begin (id)
  "Record that task ID's walkthrough is starting; return its token."
  (let ((token (list (float-time))))
    (puthash id token godel-reviewer--walkthrough-starting)
    token))

(defun godel-reviewer--walkthrough-starting-p (id)
  "Non-nil if task ID's walkthrough is starting, and hasn't hung."
  (when-let* ((token (gethash id godel-reviewer--walkthrough-starting)))
    (if (< (- (float-time) (car token)) (godel-reviewer--walkthrough-deadline))
        t
      (remhash id godel-reviewer--walkthrough-starting)
      nil)))

(defun godel-reviewer--walkthrough-current-p (id token)
  "Non-nil if TOKEN is still task ID's walkthrough, not an abandoned attempt's."
  (eq (gethash id godel-reviewer--walkthrough-starting) token))

(defun godel-reviewer--walkthrough-done (id token)
  "TOKEN's walkthrough of task ID has started, or won't.
Does nothing if a newer attempt has replaced it."
  (when (godel-reviewer--walkthrough-current-p id token)
    (remhash id godel-reviewer--walkthrough-starting)))

(defvar godel-reviewer--prepared (make-hash-table :test #'equal)
  "Task ID -> (TIME . BRIEFING): a walkthrough's briefing that couldn't be used.
Kept for the next `v w'.  Only in memory.")

(defun godel-reviewer--take-prepared (id)
  "The entry (TIME . BRIEFING) kept for task ID's next walkthrough, if fresh.
BRIEFING is nil when the author couldn't brief it: that is kept too, so the
next walkthrough opens at once without asking again.  An entry is used up
by being taken; a second walkthrough asks again."
  (when-let* ((entry (gethash id godel-reviewer--prepared)))
    (remhash id godel-reviewer--prepared)
    (and (< (- (float-time) (car entry)) godel-reviewer-prepared-max-age)
         entry)))

(defun godel-reviewer-asking-since (task)
  "When TASK's walkthrough started asking its author, or nil if it isn't."
  (gethash (plist-get task :id) godel-reviewer--asking))

(defun godel-reviewer--set-asking (id since)
  "Record (or with nil, clear) that task ID's walkthrough is asking its author."
  (if since
      (puthash id since godel-reviewer--asking)
    (remhash id godel-reviewer--asking))
  (when (fboundp 'godel-board--schedule) (godel-board--schedule)))

(defun godel-reviewer--author-session-p (task)
  "Non-nil if a reviewer of TASK should, and can, ask its author.
The author's session must have started (it has a transcript to resume)."
  (and godel-reviewer-ask-author
       (plist-get task :transcript)
       (plist-get task :session-id)))

(defun godel-reviewer-parse-briefing (output)
  "The author's briefing (a string) from a headless run's OUTPUT.
Signals an error whose message is the bare reason if there isn't one."
  (let* ((msg (cl-loop for line in (split-string output "\n" t)
                       for m = (godel-reviewer--json line)
                       when (equal (alist-get 'type m) "result") return m))
         (result (string-trim (or (alist-get 'result msg) ""))))
    (cond
     ((null msg) (error "no answer"))
     ((alist-get 'is_error msg) (error "%s" (if (string-empty-p result) "no details" result)))
     ((string-empty-p result) (error "the reply was empty")))
    result))

(defun godel-reviewer--short-reason (reason)
  "REASON cut to its first line and a reasonable width.
An API error can be a long, many-line message; the review's note and the
board's details have one line."
  (truncate-string-to-width (or (car (split-string (string-trim (or reason "")) "\n" t))
                                "no details")
                            80 nil nil "…"))

(defun godel-reviewer--briefing-done (id briefing reason)
  "Tell everyone waiting for task ID's briefing: BRIEFING, or why not.
REASON, if any, is shortened to one line."
  (let ((callbacks (reverse (cdr (gethash id godel-reviewer--briefing-runs))))
        (reason (and reason (godel-reviewer--short-reason reason))))
    (remhash id godel-reviewer--briefing-runs)
    (dolist (callback callbacks)
      ;; One waiter's error mustn't keep the others from hearing.
      (condition-case err
          (funcall callback briefing reason)
        (error (message "godel: after the author's briefing: %s" (error-message-string err)))))))

(defun godel-reviewer-briefing (task callback)
  "Ask TASK's author for a briefing for a reviewer; then call CALLBACK.
CALLBACK gets (BRIEFING REASON): the briefing text; or nil and nil if there
is nobody to ask (see `godel-reviewer--author-session-p'); or nil and the
reason if asking failed or took longer than `godel-reviewer-briefing-timeout'.
The author is a headless fork of its session, so the live one is untouched.
If the author is already being asked (a review and a walkthrough started
together), CALLBACK waits for that answer instead of asking twice.
Needs the proxy to be running already."
  (let ((id (plist-get task :id)))
    (cond
     ((not (godel-reviewer--author-session-p task))
      (funcall callback nil nil))
     ((gethash id godel-reviewer--briefing-runs)
      (push callback (cdr (gethash id godel-reviewer--briefing-runs))))
     (t
      (let* ((entry (or (godel-models-find (plist-get task :model))
                        (godel-models-find "notdiamond[1m]")))
             ;; The author's own model and effort, so the fork is its own self.
             (launch (godel-models-launch-env entry (or (plist-get task :effort)
                                                        godel-default-effort)))
             (default-directory (plist-get task :worktree))
             timed-out timer)
        ;; Marked as running before it starts, and the process only recorded
        ;; if the answer hasn't already come in, so a quick answer can't
        ;; leave the mark behind.
        (puthash id (list nil callback) godel-reviewer--briefing-runs)
        (condition-case err
            (let ((proc (godel-models--run
                         godel-claude-program
                         (append '("-p" "--output-format" "json" "--no-session-persistence")
                                 godel-reviewer--read-only-flags
                                 (list "--resume" (plist-get task :session-id) "--fork-session")
                                 (plist-get launch :args))
                         (lambda (_exit output)
                           (when timer (cancel-timer timer))
                           (let (briefing reason)
                             (if timed-out
                                 (setq reason (format "no answer after %d seconds"
                                                      godel-reviewer-briefing-timeout))
                               (condition-case err
                                   (setq briefing (godel-reviewer-parse-briefing output))
                                 (error (setq reason (error-message-string err)))))
                             (godel-reviewer--briefing-done id briefing reason)))
                         godel-reviewer--briefing-request
                         (godel-reviewer--env launch))))
              (when-let* ((run (gethash id godel-reviewer--briefing-runs)))
                (setcar run proc)
                ;; A hung author mustn't hold the review up for good.  The
                ;; process is the notdiamond wrapper, whose child is the real
                ;; `claude'; `kill-process' signals the child's whole process
                ;; group (Emacs makes it its own group leader), so both go.
                ;; Checked against the real wrapper: killing the wrapper's pid
                ;; alone orphans `claude', which keeps running.
                (setq timer (run-at-time godel-reviewer-briefing-timeout nil
                                         (lambda ()
                                           (when (process-live-p proc)
                                             (setq timed-out t)
                                             (kill-process proc)))))))
          ;; If the process can't even start, that's the reason, not a crash.
          (error (godel-reviewer--briefing-done id nil (error-message-string err)))))))))

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
  "Start a structured review of TASK on MODEL (default `godel-reviewer-model').
The author is asked for a briefing first (see `godel-reviewer-briefing')."
  (let* ((id (plist-get task :id))
         (entry (godel-reviewer--model-entry (or model godel-reviewer-model)))
         (launch (godel-models-launch-env entry godel-default-effort))
         ;; Taken now, so a task with no merge base fails at once, before it is
         ;; marked as reviewing, not later in a callback that nobody sees.
         (diff (godel-reviewer-diff-text task)))
    (when (eq (plist-get task :review-status) 'running)
      (user-error "godel: a review of %s is already running" id))
    (godel-task-update id :review-status 'running :review-started (float-time)
                       :review-activity (if (godel-reviewer--author-session-p task)
                                            "asking the author" "starting")
                       :review-seen nil :review-briefing nil
                       :review-note (format "reviewing on %s" (plist-get entry :name)))
    (godel-reviewer--start-ticking)
    (message "godel: reviewing %s on %s; the board shows its progress" id (plist-get entry :name))
    (godel-models-ensure-proxy
     (lambda (ok &optional err)
       ;; Whatever goes wrong, here or in the briefing's callback below, must
       ;; end the review, not leave it marked as running (which would refuse
       ;; every `v r' until Emacs restarts).
       (condition-case failure
           (if (not ok)
               (godel-reviewer--done id nil (or err "proxy check failed"))
             (godel-reviewer-briefing
              task
              (lambda (briefing reason)
                ;; The task may have been torn down while the author answered.
                (when-let* ((task (godel-task-get id)))
                  (condition-case err
                      (godel-reviewer--review
                       task launch briefing (godel-reviewer--fresh-diff task diff)
                       (cond (briefing "briefed by the author")
                             (reason (format "the author couldn't brief it: %s" reason))))
                    (error (godel-reviewer--done id nil (error-message-string err))))))))
         (error (godel-reviewer--done id nil (error-message-string failure))))))))

(defun godel-reviewer--review (task launch briefing diff how)
  "Run the structured review of TASK, with the author's BRIEFING (or nil).
LAUNCH is the reviewer's `godel-models-launch-env'; DIFF is the task's diff;
HOW (or nil) is added to the finished review's note, saying whether the
author's briefing was used."
  (let ((id (plist-get task :id))
        (default-directory (plist-get task :worktree)))
    (godel-task-update id :review-activity "starting" :review-briefing briefing)
    (godel-reviewer--stream
     godel-claude-program
     (append '("-p" "--output-format" "stream-json" "--verbose" "--no-session-persistence")
             godel-reviewer--read-only-flags
             (plist-get launch :args))
     (godel-reviewer-prompt task briefing diff)
     (godel-reviewer--env launch)
     (lambda (msg)
       (when-let* ((activity (godel-reviewer--activity msg))
                   (task (godel-task-get id)))
         (unless (equal activity (plist-get task :review-activity))
           (godel-task-update id :review-activity activity))))
     (lambda (_exit output)
       (condition-case err
           (godel-reviewer--done id (godel-reviewer-parse output) nil how)
         (error (godel-reviewer--done id nil (error-message-string err))))))))

;; While a review runs, redraw the board every second for its elapsed time.
(declare-function godel-board--schedule "godel-board" (&rest _))
(defvar godel-reviewer--tick-timer nil)

(defun godel-reviewer--start-ticking ()
  (unless godel-reviewer--tick-timer
    (setq godel-reviewer--tick-timer (run-with-timer 1 1 #'godel-reviewer--tick))))

(defun godel-reviewer--busy-p (task)
  "Non-nil while TASK's review runs, or its walkthrough asks its author."
  (or (eq (plist-get task :review-status) 'running)
      (godel-reviewer-asking-since task)))

(defun godel-reviewer--tick ()
  (if (cl-some #'godel-reviewer--busy-p (godel-task-all))
      (when (fboundp 'godel-board--schedule) (godel-board--schedule))
    (cancel-timer godel-reviewer--tick-timer)
    (setq godel-reviewer--tick-timer nil)))

(defun godel-reviewer--done (id findings error &optional how)
  "Store a finished review of task ID: FINDINGS, or ERROR (a string).
HOW, if given, goes after the count in the review's note."
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
                           :review-note (format "%d finding%s%s" (length new) (if (= 1 (length new)) "" "s")
                                                (if how (format " (%s)" how) "")))
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

;;;; Comment tools for the walkthrough

(declare-function claude-code-ide-make-tool "claude-code-ide-mcp-server" (&rest slots))
(declare-function claude-code-ide-mcp-server-get-session-context "claude-code-ide-mcp-server"
                  (&optional session-id))
(defvar claude-code-ide-enable-mcp-server)

(defun godel-reviewer--calling-task ()
  "The task whose walkthrough session is calling a godel tool.
Signals an error for any other caller (the author's session, or a
Claude session that isn't godel's)."
  (let* ((ctx (claude-code-ide-mcp-server-get-session-context))
         (buffer (plist-get ctx :buffer))
         (task (and ctx (plist-get ctx :project-dir)
                    (godel-task-by-directory (plist-get ctx :project-dir)))))
    (unless (and task (buffer-live-p buffer)
                 (equal (buffer-name buffer)
                        (godel-agent-buffer-name (plist-get task :worktree)
                                                 godel-reviewer-walkthrough-instance)))
      (error "These tools are only available in a godel walkthrough session"))
    task))

(defun godel-reviewer--tool-file (task file)
  "FILE relative to TASK's worktree; signals an error if it isn't there."
  (let* ((wt (file-name-as-directory (expand-file-name (plist-get task :worktree))))
         (abs (expand-file-name file wt)))
    (unless (and (string-prefix-p wt abs) (file-exists-p abs))
      (error "No file %s in the task's worktree" file))
    (file-relative-name abs wt)))

(defun godel-reviewer-tool-add-comment (file start_line end_line comment)
  "MCP tool: add a pending review comment to the task (walkthrough only)."
  (let* ((task (godel-reviewer--calling-task))
         (file (godel-reviewer--tool-file task file))
         (start (if (stringp start_line) (string-to-number start_line) start_line))
         (end (cond ((null end_line) start)
                    ((stringp end_line) (string-to-number end_line))
                    (t end_line))))
    (unless (and (integerp start) (> start 0)) (error "start_line must be a line number"))
    (when (< end start) (setq end start))
    (godel-review-add-comment (plist-get task :id) file start end
                              (concat "[walkthrough] " (string-trim comment)))
    (format "Added a pending comment on %s:%s. The reviewer (the user) sees it in the diff and decides whether to send it to the author."
            file (if (= start end) start (format "%d-%d" start end)))))

(defun godel-reviewer-tool-list-comments ()
  "MCP tool: the task's open review comments (walkthrough only)."
  (let* ((task (godel-reviewer--calling-task))
         (open (godel-review-comments task '(pending sent))))
    (if (null open)
        "No open review comments yet."
      (mapconcat (lambda (c)
                   (let ((s (plist-get c :start)) (e (plist-get c :end)))
                     (format "- %s:%s [%s] %s" (plist-get c :file)
                             (if (= s e) s (format "%d-%d" s e))
                             (plist-get c :state) (plist-get c :text))))
                 open "\n"))))

(defface godel-reviewer-focus '((t :inherit highlight :extend t))
  "Lines the walkthrough is currently showing."
  :group 'godel)

(defvar godel-reviewer--focus-overlay nil
  "Highlight of the lines the walkthrough last showed.")

(defun godel-reviewer-tool-show-location (file start_line end_line)
  "MCP tool: show FILE at START_LINE..END_LINE in the user's Emacs.
Answers at once and opens the file just after (`godel-reviewer--show'), so
anything opening it asks (a mode hook's y/n prompt, say) doesn't hold up
Claude's tool call."
  (let* ((task (godel-reviewer--calling-task))
         (file (godel-reviewer--tool-file task file))
         (start (if (stringp start_line) (string-to-number start_line) start_line))
         (end (cond ((null end_line) start)
                    ((stringp end_line) (string-to-number end_line))
                    (t end_line)))
         (session (plist-get (claude-code-ide-mcp-server-get-session-context) :buffer)))
    (unless (and (integerp start) (> start 0)) (error "start_line must be a line number"))
    (when (< end start) (setq end start))
    (run-at-time 0 nil #'godel-reviewer--show
                 (expand-file-name file (plist-get task :worktree)) start end session)
    (format "Showing %s:%s in the user's Emacs (highlighted)."
            file (if (= start end) start (format "%d-%d" start end)))))

(defun godel-reviewer--show (path start end session)
  "Show PATH at lines START..END, highlighted, beside SESSION's window.
Uses a content window of the frame showing SESSION (see
`godel-agent-content-window'; never a Claude window) and leaves the user's cursor where it is."
  (let* ((buffer (find-file-noselect path))
         (session-window (and (buffer-live-p session) (get-buffer-window session t)))
         (frame (if session-window (window-frame session-window) (selected-frame)))
         (window (with-selected-frame frame
                   (or (let ((content (godel-agent-content-window t)))
                         (and content (not (eq content session-window)) content))
                       (display-buffer buffer '(nil (inhibit-same-window . t)))))))
    (set-window-buffer window buffer)
    (with-current-buffer buffer
      (let ((beg (save-excursion (goto-char (point-min)) (forward-line (1- start)) (point)))
            (stop (save-excursion (goto-char (point-min)) (forward-line end) (point))))
        (unless (overlayp godel-reviewer--focus-overlay)
          (setq godel-reviewer--focus-overlay (make-overlay beg stop buffer))
          (overlay-put godel-reviewer--focus-overlay 'face 'godel-reviewer-focus))
        (move-overlay godel-reviewer--focus-overlay beg stop buffer)
        (set-window-point window beg)
        (with-selected-window window (recenter (max 2 (/ (window-body-height) 4))))))))

(defun godel-reviewer--register-tools ()
  "Register godel's walkthrough tools on claude-code-ide's MCP tools server."
  (claude-code-ide-make-tool
   :function #'godel-reviewer-tool-add-comment
   :name "godel_add_comment"
   :description "Add a review comment on lines of a file in this task's worktree. The comment goes to the user's review list as pending; the user decides whether to send it to the author of the change. Use it for real problems you find while walking through the change, and whenever the user asks you to note something or agrees that something should change. Write the comment for the author: what's wrong and what to do. Only available in a godel walkthrough session."
   :args '((:name "file" :type string :description "Path relative to the repository root (or absolute inside the worktree).")
           (:name "start_line" :type integer :description "First line, in the file as it is now.")
           (:name "end_line" :type integer :description "Last line (defaults to start_line)." :optional t)
           (:name "comment" :type string :description "The comment for the author."))
   :category "godel")
  (claude-code-ide-make-tool
   :function #'godel-reviewer-tool-show-location
   :name "godel_show_location"
   :description "Show lines of a file in this task's worktree to the user: opens the file in their Emacs next to this session and highlights the lines, without taking their cursor. Use it for each place you're about to talk about. Only available in a godel walkthrough session."
   :args '((:name "file" :type string :description "Path relative to the repository root (or absolute inside the worktree).")
           (:name "start_line" :type integer :description "First line to show, in the file as it is now.")
           (:name "end_line" :type integer :description "Last line (defaults to start_line)." :optional t))
   :category "godel")
  (claude-code-ide-make-tool
   :function #'godel-reviewer-tool-list-comments
   :name "godel_list_comments"
   :description "List this task's open review comments (pending or sent), so you don't add duplicates. Only available in a godel walkthrough session."
   :category "godel"))

(with-eval-after-load 'claude-code-ide-mcp-server
  (godel-reviewer--register-tools))

(defconst godel-reviewer--walkthrough-tools
  '("mcp__emacs-tools__godel_add_comment" "mcp__emacs-tools__godel_list_comments"
    "mcp__emacs-tools__godel_show_location")
  "godel's tools as Claude names them; allowed without asking in walkthroughs.")

(defun godel-reviewer-walkthrough-prompt (task &optional briefing diff)
  "The walkthrough's first message for TASK, with the author's BRIEFING if any.
DIFF is the task's diff if already taken (`godel-reviewer-diff-text')."
  (format "Walk me through the changes in this worktree, one hunk at a time.
%sThey're against %s, new files included:
```diff
%s```
For each hunk: show it to me with the godel_show_location tool (file and
lines as they are now; it opens them in my editor next to this chat),
explain what changed and why it matters, point out anything questionable,
then stop and wait for me before the next hunk. Don't change any files.

Record review comments with the godel_add_comment tool (file, lines as they
are now, and a comment written for the author: what's wrong and what to
do). Add one on your own whenever you find a real problem, and whenever I
ask you to note something or agree that something should change. Say
briefly what you noted. Check godel_list_comments first so you don't add
duplicates."
          (let ((section (godel-reviewer--briefing-section briefing)))
            (if (string-empty-p section) "" (concat "\n" section)))
          (plist-get task :base) (or diff (godel-reviewer-diff-text task))))

(defun godel-reviewer--workspace ()
  "The name of the current Doom workspace, or nil without workspaces."
  (and (fboundp '+workspace-current-name) (ignore-errors (+workspace-current-name))))

(defun godel-reviewer--can-open-here-p (origin)
  "Non-nil if a walkthrough that waited may open now, without disturbing you.
Not if you've moved to another workspace than ORIGIN, where `v w' was
pressed (godel doesn't switch workspaces under you: that changes where
`+workspace/other' goes and runs the workspace hooks), nor while you're in
the minibuffer (a prompt, `M-x'), where a new window would be a surprise."
  (and (equal (godel-reviewer--workspace) origin)
       (not (active-minibuffer-window))))

(defun godel-reviewer--walkthrough-buffer (task)
  "TASK's walkthrough session buffer if it's running, else nil."
  (let ((buffer (get-buffer (godel-agent-buffer-name (plist-get task :worktree)
                                                     godel-reviewer-walkthrough-instance))))
    (and (buffer-live-p buffer) (process-live-p (get-buffer-process buffer)) buffer)))

(defun godel-reviewer--start-session (task launch callback &optional focus)
  "Open the walkthrough session for TASK and call CALLBACK with its buffer.
CALLBACK gets the buffer once its input box is showing; `:not-ready' if the
session started but never showed it; nil if it didn't start or closed first
(a message has said so).  The session takes the focus only if FOCUS: when it
opens some time after you asked, what you're typing then must not land in it."
  (let* ((default-directory (plist-get task :worktree))
         ;; No GODEL_SERVER and no godel hooks: this session must not
         ;; report as the task.
         (process-environment (godel-reviewer--env launch))
         (claude-code-ide-cli-path godel-claude-program)
         (claude-code-ide-cli-extra-flags
          ;; Read-only by taking away the tools that change things,
          ;; not by plan mode: plan mode asks before every MCP tool
          ;; call, godel's comment tools included.
          (mapconcat #'shell-quote-argument
                     (append '("--permission-mode" "default")
                             godel-reviewer--read-only-flags
                             (list "--allowedTools"
                                   (string-join godel-reviewer--walkthrough-tools ","))
                             (plist-get launch :args))
                     " "))
         (claude-code-ide-focus-on-open focus)
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
          (progn (message "godel: the walkthrough session didn't start")
                 (funcall callback nil))
        (godel-tui-wait buffer #'godel-tui-input-ready-p 30
                        (lambda (screen)
                          (cond (screen (funcall callback buffer))
                                ((buffer-live-p buffer) (funcall callback :not-ready))
                                (t (message "godel: the walkthrough session closed before it was ready")
                                   (funcall callback nil)))))))))

(defun godel-reviewer--send-prompt (buffer prompt)
  "Type PROMPT into the session in BUFFER and press return.
Does nothing if you've closed the session meanwhile."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ghostel-paste-string prompt)
      (run-with-timer 0.2 nil (lambda ()
                                (when (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (ghostel-send-key "return"))))))))

(defun godel-reviewer--open-walkthrough (task launch briefing diff origin waited token)
  "Open TASK's walkthrough session, then send its first message.
The message carries BRIEFING (or nil) and DIFF.  If WAITED (for the author)
the session doesn't take the focus, which would send what you're typing
somewhere else, and a notification says when it's ready.  If WAITED and you've
moved on from ORIGIN, where `v w' was pressed, or are in the minibuffer,
nothing is opened under you: the briefing is kept, and the notification says
to press `v w' where you were.  Nothing is typed into the session before its
input box is ready."
  (let* ((id (plist-get task :id))
         (title (plist-get task :title)))
    (cond
     ((and waited (not (godel-reviewer--can-open-here-p origin)))
      ;; Kept even without a briefing, so the next `v w' doesn't ask again.
      (puthash id (cons (float-time) briefing) godel-reviewer--prepared)
      (godel-reviewer--walkthrough-done id token)
      (godel-notify (format "godel: walkthrough of %s is ready to start" title)
                    (format "Press v w%s to open it%s."
                            (if origin (format " in %s" origin) "")
                            (if briefing ""
                              " (the author couldn't brief it, so it opens without one)"))))
     (t
      (condition-case err
          (godel-reviewer--start-session
           task launch
           (lambda (buffer)
             (godel-reviewer--walkthrough-done id token)
             (cond
              ((not (godel-task-get id)))     ; torn down meanwhile
              ((null buffer))                 ; never started: already said so
              ((eq buffer :not-ready)
               (message "godel: the walkthrough session isn't ready; type your request there"))
              (t (godel-reviewer--send-prompt
                  buffer (godel-reviewer-walkthrough-prompt
                          (godel-task-get id) briefing
                          (godel-reviewer--fresh-diff (godel-task-get id) diff)))
                 (when waited
                   (godel-notify (format "godel: walkthrough of %s is ready" title)
                                 "Its first message has been sent.")))))
           (not waited))
        (error (godel-reviewer--walkthrough-done id token)
               (message "godel: the walkthrough couldn't start: %s"
                        (error-message-string err))))))))

(defun godel-reviewer--start-walkthrough (task model)
  "Start TASK's walkthrough on MODEL: ask the author, then open the session."
  (unless (bound-and-true-p claude-code-ide-enable-mcp-server)
    (message "godel: the walkthrough can't record comments: claude-code-ide's MCP tools are off (see `claude-code-ide-emacs-tools-setup')"))
  (let* ((id (plist-get task :id))
         (entry (godel-reviewer--model-entry (or model godel-reviewer-model)))
         (launch (godel-models-launch-env entry godel-default-effort))
         ;; Taken now, so a task with no merge base fails at once, before the
         ;; author is asked, not later in a callback.
         (diff (godel-reviewer-diff-text task))
         (origin (godel-reviewer--workspace))
         ;; A briefing kept from a walkthrough that couldn't open: no need to
         ;; ask again.
         (prepared (godel-reviewer--take-prepared id))   ; (TIME . BRIEFING-or-nil)
         (waited (and (not prepared) (godel-reviewer--author-session-p task)))
         (token (godel-reviewer--walkthrough-begin id)))
    (godel-models-ensure-proxy
     (lambda (ok &optional err)
       ;; Whatever goes wrong must free the next `v w', not block it for good.
       (condition-case failure
           (cond
            ((not (godel-reviewer--walkthrough-current-p id token)))  ; given up on
            ((not ok)
             (godel-reviewer--walkthrough-done id token)
             (message "godel: walkthrough not started: %s" (or err "proxy check failed")))
            (prepared
             (godel-reviewer--open-walkthrough task launch (cdr prepared) diff origin nil token))
            (t
             (when waited
               (godel-reviewer--set-asking id (float-time))
               (godel-reviewer--start-ticking)
               (message "godel: asking the author for a briefing; the walkthrough opens when it's back"))
             (godel-reviewer-briefing
              task
              (lambda (briefing reason)
                (godel-reviewer--set-asking id nil)
                (if-let* ((task (and (godel-reviewer--walkthrough-current-p id token)
                                     (godel-task-get id))))
                    (progn
                      (when reason
                        (message "godel: the author couldn't brief the walkthrough (%s); using the task's prompt and the diff"
                                 reason))
                      (godel-reviewer--open-walkthrough task launch briefing diff origin waited token))
                  (godel-reviewer--walkthrough-done id token))))))
         (error (godel-reviewer--set-asking id nil)
                (godel-reviewer--walkthrough-done id token)
                (message "godel: the walkthrough couldn't start: %s"
                         (error-message-string failure))))))))

(defun godel-reviewer-walkthrough (task &optional model)
  "Start an interactive, read-only walkthrough of TASK's changes.
The author is asked for a briefing first (see `godel-reviewer-briefing').
While it's being written the board shows \"asking the author\", as it does
for a review.  Then the session opens, and its first message, which carries
the briefing, is sent once its input box is ready.  Having waited, the
session opens without taking the focus and a notification says when it's
ready; if you've moved to another workspace or are in the minibuffer by
then, nothing opens under you: the briefing is kept and the next `v w' in
the workspace you started in opens at once.  Pressing it again while it's
starting is refused; once the session is open, it's shown instead."
  (require 'claude-code-ide)
  (require 'claude-code-ide-mcp-server nil t)
  (let ((id (plist-get task :id)))
    (cond
     ((godel-reviewer--walkthrough-starting-p id)
      (user-error "godel: the walkthrough of %s is already starting" id))
     ((godel-reviewer--walkthrough-buffer task)
      (pop-to-buffer (godel-reviewer--walkthrough-buffer task))
      (message "godel: the walkthrough of %s is already open" id))
     (t (godel-reviewer--start-walkthrough task model)))))

(provide 'godel-reviewer)
;;; godel-reviewer.el ends here
