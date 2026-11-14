;;; godel-ship.el --- Commit, push and open a PR for a task -*- lexical-binding: t -*-

;;; Commentary:
;; Shipping a task (PLAN.md, "Ship" and the Phase 2 decisions).
;;
;; `godel-ship' (P on the board):
;;   1. The author drafts the commit message and PR description from its own
;;      conversation: a headless run of a *fork* of its session
;;      (`--resume <id> --fork-session -p', read-only, not saved), so the
;;      live session is untouched.  A task whose session never started gets
;;      a draft from the diff alone.
;;   2. You edit the draft in a compose buffer: the commit message on top,
;;      then a separator line, then the PR title and description.  The
;;      buffer opens at once and shows the drafting's progress (elapsed time)
;;      until the draft arrives; the board row shows it too.  Cancelling
;;      (:q!) while it drafts stops the draft.
;;   3. `:wq' commits (if there are changes), pushes, and opens the PR with
;;      `gh' (a draft PR with `godel-ship' DRAFT, P d on the board).  An
;;      open PR that's a draft when you ship it as ready (or the other way
;;      round) is marked ready (or converted to draft) after the push.  The PR URL is saved on the task.  If the task already has a
;;      PR, only the commit message is drafted and the push updates the PR.
;; Every git and gh command is logged in *godel-ship: ID*.  Afterwards, if
;; the author's session is running and idle, godel tells it what was
;; shipped (commit, PR URL), so it knows if you keep working with it.
;; `godel-ship-browse-pr' opens a task's PR (gp on the board, o in v).
;;
;; `godel-ship-check-merged' asks gh about tasks' PRs; a merged PR moves
;; its task to the `merged' state.  It runs when the board opens and every
;; `godel-ship-check-interval' seconds while `godel-mode' is on.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'godel-task)
(require 'godel-models)
(require 'godel-worktree)
(require 'godel-compose)
(require 'godel-review)
(require 'godel-agent)

(defcustom godel-ship-remote "origin"
  "Git remote that task branches are pushed to."
  :group 'godel
  :type 'string)

(defcustom godel-gh-program "gh"
  "The GitHub CLI."
  :group 'godel
  :type 'string)

(defcustom godel-ship-check-interval 300
  "Seconds between checks for merged PRs (nil to only check on the board)."
  :group 'godel
  :type '(choice integer (const nil)))

(defconst godel-ship-separator
  "==== Pull request: title on the next line, then the description ===="
  "Line separating the commit message from the PR in the ship draft.")

;;;; Drafting

(defun godel-ship--draft-prompt (task stat has-pr)
  (concat
   (format "Draft %s for the changes you made in this task. Don't change any files.\n\n"
           (if has-pr "a commit message" "a commit message and a pull request"))
   "The changes, against " (plist-get task :base) ":\n" stat "\n\n"
   "Reply with exactly these parts and nothing else:\n"
   "<commit>A short summary line (under 72 characters), a blank line, then a few lines saying what changed and why.</commit>\n"
   (unless has-pr
     "<pr-title>The pull request title.</pr-title>\n<pr-body>The pull request description, in Markdown: what changed, why, and how it was checked.</pr-body>\n")))

(defun godel-ship-parse-draft (output)
  "(COMMIT PR-TITLE PR-BODY) from a headless run's OUTPUT; missing parts are nil."
  (let* ((line (cl-find-if (lambda (l) (string-prefix-p "{" l)) (split-string output "\n" t)))
         (msg (and line (ignore-errors (json-parse-string line :object-type 'alist
                                                            :null-object nil :false-object nil))))
         (result (or (alist-get 'result msg) ""))
         (part (lambda (tag)
                 (when (string-match (format "<%s>\\(\\(?:.\\|\n\\)*?\\)</%s>" tag tag) result)
                   (string-trim (match-string 1 result))))))
    (when (alist-get 'is_error msg)
      (error "The author couldn't draft it: %s" result))
    (list (funcall part "commit") (funcall part "pr-title") (funcall part "pr-body"))))

(defvar godel-ship--drafting (make-hash-table :test #'equal)
  "Task ID -> (START . PROCESS) for drafts in progress.
Only in memory: a draft doesn't survive a restart, so neither does this.")

(defun godel-ship-drafting (task)
  "When TASK's ship draft is in progress, the time it started."
  (car (gethash (plist-get task :id) godel-ship--drafting)))

(defun godel-ship-draft (task callback)
  "Ask TASK's author for a draft; call CALLBACK with (COMMIT PR-TITLE PR-BODY).
CALLBACK gets nil and an error message if drafting fails.  While it runs,
`godel-ship-drafting' says since when."
  (let* ((default-directory (plist-get task :worktree))
         (stat (or (godel-task--git default-directory "status" "--short") ""))
         (has-pr (godel-ship-open-pr task))
         (entry (or (godel-models-find (plist-get task :model)) (godel-models-find "notdiamond[1m]")))
         (launch (godel-models-launch-env entry (or (plist-get task :effort) godel-default-effort)))
         (resume (and (plist-get task :transcript) (plist-get task :session-id)))
         (prompt (concat (godel-ship--draft-prompt task stat has-pr)
                         (unless resume
                           (concat "\nThe diff:\n```diff\n"
                                   (with-temp-buffer
                                     (let ((process-environment
                                            (cons (concat "GIT_INDEX_FILE=" (godel-review--rebuild-index task))
                                                  process-environment)))
                                       (process-file "git" nil t nil "diff" "--no-ext-diff"
                                                     (or (godel-review-merge-base task) "HEAD")))
                                     (buffer-string))
                                   "```\n")))))
    (puthash (plist-get task :id) (list (float-time)) godel-ship--drafting)
    (setq callback (let ((id (plist-get task :id)) (callback callback))
                     (lambda (&rest args)
                       (when (gethash id godel-ship--drafting)
                         (remhash id godel-ship--drafting)
                         (apply callback args)))))
    (godel-models-ensure-proxy
     (lambda (ok &optional err)
       (if (not ok)
           (funcall callback nil (or err "proxy check failed"))
         (let ((default-directory (plist-get task :worktree))
               (entry (gethash (plist-get task :id) godel-ship--drafting)))
           (when entry
             (setcdr entry
                     (godel-models--run
                      godel-claude-program
                      (append '("-p" "--output-format" "json" "--no-session-persistence"
                                "--disallowedTools" "Edit,Write,MultiEdit,NotebookEdit,Bash")
                              (when resume (list "--resume" resume "--fork-session"))
                              (plist-get launch :args))
                      (lambda (_exit output)
                        (condition-case e
                            (funcall callback (godel-ship-parse-draft output))
                          (error (funcall callback nil (error-message-string e)))))
                      prompt
                      (cons "GODEL_SERVER" (plist-get launch :env)))))))))))

(defun godel-ship-cancel-draft (id)
  "Stop task ID's ship draft, if one is running; its callback isn't called."
  (when-let* ((entry (gethash id godel-ship--drafting)))
    (remhash id godel-ship--drafting)
    (when (process-live-p (cdr entry)) (delete-process (cdr entry)))))

(defun godel-ship-draft-text (draft has-pr)
  "The compose text for DRAFT (COMMIT PR-TITLE PR-BODY)."
  (pcase-let ((`(,commit ,title ,body) draft))
    (concat (or commit "") "\n"
            (unless has-pr
              (concat "\n" godel-ship-separator "\n" (or title "") "\n\n" (or body "") "\n")))))

(defun godel-ship-split (text)
  "Split the edited TEXT into (COMMIT PR-TITLE PR-BODY)."
  (let* ((parts (split-string text (concat "^" (regexp-quote godel-ship-separator) "$")))
         (commit (string-trim (car parts)))
         (pr (and (cdr parts) (string-trim (cadr parts))))
         (pr-lines (and pr (split-string pr "\n"))))
    (list (unless (string-empty-p commit) commit)
          (and pr-lines (string-trim (car pr-lines)))
          (and pr-lines (string-trim (string-join (cdr pr-lines) "\n"))))))

;;;; Shipping

(defun godel-ship--log (task)
  (format "*godel-ship: %s*" (plist-get task :id)))

(defun godel-ship--run (task command callback)
  "Run COMMAND (a list) in TASK's worktree; CALLBACK gets (EXIT OUTPUT)."
  (let* ((default-directory (plist-get task :worktree))
         (log (get-buffer-create (godel-ship--log task)))
         (out (generate-new-buffer " *godel-ship-out*")))
    (with-current-buffer log
      (goto-char (point-max))
      (insert (format "$ %s\n" (mapconcat #'shell-quote-argument command " "))))
    (make-process
     :name "godel-ship" :buffer out :noquery t :connection-type 'pipe
     :command command
     :sentinel (lambda (p _)
                 (unless (process-live-p p)
                   (let ((text (with-current-buffer out (buffer-string))))
                     (kill-buffer out)
                     (with-current-buffer log
                       (goto-char (point-max))
                       (insert text (if (string-suffix-p "\n" text) "" "\n")))
                     (funcall callback (process-exit-status p) text)))))))

(defvar godel-ship--last-output nil "Output of the last ship step that ran.")

(defun godel-ship--steps (task steps done)
  "Run STEPS (each (DESCRIPTION . COMMAND-FN)) in order; stop at a failure.
COMMAND-FN returns a command list, or nil to skip the step.  DONE gets
nil, or an error message naming the step that failed."
  (if (null steps)
      (funcall done nil)
    (pcase-let* ((`(,what . ,fn) (car steps))
                 (command (funcall fn)))
      (if (not command)
          (godel-ship--steps task (cdr steps) done)
        (godel-task-update (plist-get task :id) :last-event (concat what "…"))
        (godel-ship--run task command
                         (lambda (exit output)
                           (if (zerop exit)
                               (progn (setq godel-ship--last-output output)
                                      (godel-ship--steps task (cdr steps) done))
                             (funcall done (format "%s failed; see %s" what (godel-ship--log task))))))))))

(defun godel-ship-execute (task commit pr-title pr-body &optional draft)
  "Commit (with COMMIT if there are changes), push, and open a PR for TASK.
With DRAFT, a draft PR.  If TASK already has an open PR, make it a draft
or ready to match DRAFT."
  (let* ((id (plist-get task :id))
         (draft (and draft t))
         (existing (godel-ship-open-pr task))
         (toggle (and existing (not (eq draft (and (plist-get task :pr-draft) t)))))
         (dir (make-temp-file "godel-ship" t))
         (msg-file (expand-file-name "commit.txt" dir))
         (body-file (expand-file-name "pr.md" dir))
         (changes (not (string-empty-p (or (godel-task--git (plist-get task :worktree)
                                                            "status" "--porcelain")
                                           ""))))
         (open-pr (and (not (godel-ship-open-pr task)) pr-title)))
    (when (and changes (not commit))
      (user-error "godel: there are uncommitted changes but no commit message"))
    (when (and (not (godel-ship-open-pr task)) (not pr-title))
      (user-error "godel: no PR title (the line after the separator)"))
    (with-temp-file msg-file (insert (or commit "") "\n"))
    (with-temp-file body-file (insert (or pr-body "") "\n"))
    (godel-ship--steps
     task
     `(("staging" . ,(lambda () (and changes '("git" "add" "--all"))))
       ("committing" . ,(lambda () (and changes (list "git" "commit" "--file" msg-file))))
       ("pushing" . ,(lambda () (list "git" "push" "--set-upstream" godel-ship-remote
                                      (plist-get task :branch))))
       ("opening the PR" . ,(lambda () (and open-pr
                                            (append
                                             (list godel-gh-program "pr" "create"
                                                   "--base" (plist-get task :base)
                                                   "--head" (plist-get task :branch)
                                                   "--title" pr-title "--body-file" body-file)
                                             (when draft '("--draft"))))))
       (,(if draft "converting the PR to draft" "marking the PR ready")
        . ,(lambda () (and toggle (append (list godel-gh-program "pr" "ready" existing)
                                          (when draft '("--undo")))))))
     (lambda (err)
       (delete-directory dir t)
       (if err
           (progn (godel-task-update id :last-event err)
                  (message "godel: %s" err))
         (let ((url (and open-pr godel-ship--last-output
                         (string-match "https://[^ \t\n]+" godel-ship--last-output)
                         (match-string 0 godel-ship--last-output))))
           (apply #'godel-task-update id
                  :last-event (cond (url (concat (if draft "Draft PR opened: " "PR opened: ") url))
                                    (open-pr "pushed; PR created (no URL printed)")
                                    (toggle (if draft "pushed; PR converted to draft"
                                              "pushed; PR marked ready for review"))
                                    (t "pushed"))
                  (append
                   ;; A new PR replaces a closed or merged one; a merged
                   ;; task is back under review.
                   (when url
                     `(:pr-url ,url :pr-state open :pr-draft ,draft
                               ,@(when (eq (plist-get (godel-task-get id) :state) 'merged)
                                   '(:state needs-review))))
                   (when toggle `(:pr-draft ,draft))))
           (godel-ship--tell-author (godel-task-get id) (and changes commit) (and url t))
           (message "godel: %s %s" id (if url (concat "PR opened: " url) "pushed"))))))))

(defun godel-ship-note (task commit hash opened)
  "What to tell TASK's author after shipping.
COMMIT is the commit message (nil if nothing was committed), HASH the
pushed commit, OPENED non-nil if this ship opened the PR."
  (let ((pr (plist-get task :pr-url)))
    (concat "For your information, no action needed: I shipped this task from my editor. "
            (if commit
                (format "I committed your changes as %s (\"%s\") and pushed %s to %s"
                        hash (car (split-string commit "\n")) (plist-get task :branch) godel-ship-remote)
              (format "I pushed %s (at %s) to %s" (plist-get task :branch) hash godel-ship-remote))
            (cond ((and pr opened) (format ", and opened a %spull request: %s."
                                           (if (plist-get task :pr-draft) "draft " "") pr))
                  (pr (format ", which updates the pull request %s." pr))
                  (t "."))
            " Reply with just \"Noted.\"")))

(defun godel-ship--tell-author (task commit opened)
  "Tell TASK's author what was shipped, if its session is running and idle."
  (when (and (godel-agent-live-p task)
             (not (memq (plist-get task :state) '(working setting-up))))
    (let ((hash (string-trim (or (godel-task--git (plist-get task :worktree)
                                                  "rev-parse" "--short" "HEAD")
                                 ""))))
      (godel-agent-send task (godel-ship-note task commit hash opened)))))

(defun godel-ship-pr-state (task)
  "TASK's PR state as godel last saw it: `open', `closed', `merged', or nil
without a PR.  (Tasks shipped before godel kept the state count as open,
or merged if the task is.)"
  (when (plist-get task :pr-url)
    (or (plist-get task :pr-state)
        (if (eq (plist-get task :state) 'merged) 'merged 'open))))

(defun godel-ship-open-pr (task)
  "TASK's PR URL if that PR is still open, else nil.
A ship with no open PR opens a new one."
  (and (eq (godel-ship-pr-state task) 'open) (plist-get task :pr-url)))

(defun godel-ship-pr-number (task)
  "TASK's PR number as a string, or nil."
  (when-let* ((url (plist-get task :pr-url)))
    (and (string-match "/pull/\\([0-9]+\\)" url) (match-string 1 url))))

;;;###autoload
(defun godel-ship-browse-pr (task)
  "Open TASK's pull request in the browser."
  (browse-url (or (plist-get task :pr-url)
                  (user-error "godel: %s has no PR yet (P to ship it)" (plist-get task :id)))))

;;;###autoload
(declare-function godel-board--schedule "godel-board" (&rest _))

(defun godel-ship--wait-status (what start)
  (format "⟳ The author is drafting the %s… %ds\n\n  It appears here when it's ready.  :q! cancels."
          what (round (- (float-time) start))))

(defun godel-ship (task &optional draft)
  "Draft (with the author), edit, then commit, push and open a PR for TASK.
With DRAFT, the PR is a draft (an open PR is converted to one).  The
compose buffer opens at once and shows the drafting's progress."
  (let* ((id (plist-get task :id))
         (has-pr (godel-ship-open-pr task))
         (what (if has-pr "commit message" "commit message and PR"))
         (title (format "Ship %s: commit message%s" id
                        (cond ((not has-pr)
                               (format ", then the %sPR below the separator" (if draft "draft " "")))
                              ((not (eq (and draft t) (and (plist-get task :pr-draft) t)))
                               (format ", then push to %s and %s" has-pr
                                       (if draft "convert it to a draft" "mark it ready")))
                              (t (format ", then push to %s" has-pr)))))
         (buffer (godel-compose
                  (plist-get task :repo) 'ship (concat title "   (drafting…)")
                  (lambda (text)
                    (pcase-let ((`(,commit ,title ,body) (godel-ship-split text)))
                      (godel-ship-execute (godel-task-get id) commit title body draft)))))
         (start (float-time))
         (timer nil))
    (godel-compose-wait buffer (godel-ship--wait-status what start))
    ;; Each second: the elapsed time in the buffer and on the board.
    (setq timer (run-with-timer
                 1 1 (lambda ()
                       (if (not (buffer-live-p buffer))
                           (progn (cancel-timer timer) (godel-ship-cancel-draft id)
                                  (when (fboundp 'godel-board--schedule) (godel-board--schedule)))
                         (godel-compose-wait buffer (godel-ship--wait-status what start))
                         (when (fboundp 'godel-board--schedule) (godel-board--schedule))))))
    (godel-ship-draft
     task
     (lambda (draft &optional err)
       (cancel-timer timer)
       (when (fboundp 'godel-board--schedule) (godel-board--schedule))
       (godel-compose-fill buffer (godel-ship-draft-text (or draft '(nil nil nil)) has-pr)
                           (format "%s   (%s)" title
                                   (if draft "drafted by the author" (format "no draft: %s" err))))
       (godel-notify (format "godel: the %s draft for %s is ready" what id)
                     (if draft "Edit it, then :wq to ship" (format "No draft: %s" err)))))
    buffer))

;;;; Merged PRs

(defun godel-ship-check-merged ()
  "Ask gh about every task PR that isn't merged; save its state (`:pr-state',
`:pr-draft').  Merged moves the task to the `merged' state; closed (without
merging), reopened, and draft <-> ready are noted on the task."
  (interactive)
  (dolist (task (godel-task-all))
    (when (and (memq (godel-ship-pr-state task) '(open closed))
               (executable-find godel-gh-program))
      (let ((id (plist-get task :id)))
        (godel-models--run
         godel-gh-program (list "pr" "view" (plist-get task :pr-url) "--json" "state,isDraft")
         (lambda (_exit out)
           (let* ((json (ignore-errors (json-parse-string (string-trim out) :object-type 'alist
                                                          :false-object nil :null-object nil)))
                  (state (pcase (alist-get 'state json)
                           ("OPEN" 'open) ("CLOSED" 'closed) ("MERGED" 'merged)))
                  (draft (and (alist-get 'isDraft json) t))
                  (task (godel-task-get id)))
             (when (and task state)
               (godel-ship--note-pr-state task state draft)))))))))

(defun godel-ship--note-pr-state (task state draft)
  "Save what GitHub says about TASK's PR: STATE and whether it's a DRAFT."
  (let* ((id (plist-get task :id))
         (was (godel-ship-pr-state task))
         (known-draft (plist-member task :pr-draft))
         (was-draft (and (plist-get task :pr-draft) t))
         (event (cond ((and (eq state 'merged) (not (eq was 'merged))) "PR merged")
                      ((and (eq state 'closed) (not (eq was 'closed))) "PR closed without merging")
                      ((and (eq state 'open) (eq was 'closed)) "PR reopened")
                      ((and (eq state 'open) known-draft (not (eq draft was-draft)))
                       (if draft "PR converted to draft" "PR marked ready for review")))))
    (when (or (not (eq state (plist-get task :pr-state))) (not known-draft)
              (not (eq draft was-draft)))
      (apply #'godel-task-update id :pr-state state :pr-draft draft
             (append (when (eq state 'merged) '(:state merged))
                     (when event (list :last-event event)))))))

(defvar godel-ship--timer nil)

(defun godel-ship-start-checking ()
  (godel-ship-stop-checking)
  (when godel-ship-check-interval
    (setq godel-ship--timer (run-with-timer godel-ship-check-interval godel-ship-check-interval
                                            #'godel-ship-check-merged))))

(defun godel-ship-stop-checking ()
  (when godel-ship--timer (cancel-timer godel-ship--timer))
  (setq godel-ship--timer nil))

(provide 'godel-ship)
;;; godel-ship.el ends here
