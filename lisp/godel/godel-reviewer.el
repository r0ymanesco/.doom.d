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
Uses the main window of the frame showing SESSION (not SESSION's own
window) and leaves the user's cursor where it is."
  (let* ((buffer (find-file-noselect path))
         (session-window (and (buffer-live-p session) (get-buffer-window session t)))
         (frame (if session-window (window-frame session-window) (selected-frame)))
         (window (with-selected-frame frame
                   (let ((main (window-main-window)))
                     (if (and (window-live-p main) (not (eq main session-window)))
                         main
                       (display-buffer buffer '(nil (inhibit-same-window . t))))))))
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

(defun godel-reviewer-walkthrough-prompt (task)
  (format "Walk me through the changes in this worktree, one hunk at a time.
They're against %s, new files included:
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
          (plist-get task :base) (godel-reviewer-diff-text task)))

(defun godel-reviewer-walkthrough (task &optional model)
  "Start an interactive, read-only walkthrough of TASK's changes."
  (require 'claude-code-ide)
  (require 'claude-code-ide-mcp-server nil t)
  (unless (bound-and-true-p claude-code-ide-enable-mcp-server)
    (message "godel: the walkthrough can't record comments: claude-code-ide's MCP tools are off (see `claude-code-ide-emacs-tools-setup')"))
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
