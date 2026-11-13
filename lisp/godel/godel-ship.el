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
;;      `gh'.  The PR URL is saved on the task.  If the task already has a
;;      PR, only the commit message is drafted and the push updates the PR.
;; Every git and gh command is logged in *godel-ship: ID*.
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
         (has-pr (plist-get task :pr-url))
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

(defun godel-ship-execute (task commit pr-title pr-body)
  "Commit (with COMMIT if there are changes), push, and open a PR for TASK."
  (let* ((id (plist-get task :id))
         (dir (make-temp-file "godel-ship" t))
         (msg-file (expand-file-name "commit.txt" dir))
         (body-file (expand-file-name "pr.md" dir))
         (changes (not (string-empty-p (or (godel-task--git (plist-get task :worktree)
                                                            "status" "--porcelain")
                                           ""))))
         (open-pr (and (not (plist-get task :pr-url)) pr-title)))
    (when (and changes (not commit))
      (user-error "godel: there are uncommitted changes but no commit message"))
    (when (and (not (plist-get task :pr-url)) (not pr-title))
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
                                            (list godel-gh-program "pr" "create"
                                                  "--base" (plist-get task :base)
                                                  "--head" (plist-get task :branch)
                                                  "--title" pr-title "--body-file" body-file)))))
     (lambda (err)
       (delete-directory dir t)
       (if err
           (progn (godel-task-update id :last-event err)
                  (message "godel: %s" err))
         (let ((url (and open-pr godel-ship--last-output
                         (string-match "https://[^ \t\n]+" godel-ship--last-output)
                         (match-string 0 godel-ship--last-output))))
           (godel-task-update id :pr-url (or url (plist-get (godel-task-get id) :pr-url))
                              :last-event (cond (url (concat "PR opened: " url))
                                                (open-pr "pushed; PR created (no URL printed)")
                                                (t "pushed")))
           (message "godel: %s %s" id (if url (concat "PR opened: " url) "pushed"))))))))

;;;###autoload
(declare-function godel-board--schedule "godel-board" (&rest _))

(defun godel-ship--wait-status (what start)
  (format "⟳ The author is drafting the %s… %ds\n\n  It appears here when it's ready.  :q! cancels."
          what (round (- (float-time) start))))

(defun godel-ship (task)
  "Draft (with the author), edit, then commit, push and open a PR for TASK.
The compose buffer opens at once and shows the drafting's progress."
  (let* ((id (plist-get task :id))
         (has-pr (plist-get task :pr-url))
         (what (if has-pr "commit message" "commit message and PR"))
         (title (format "Ship %s: commit message%s" id
                        (if has-pr (format ", then push to %s" has-pr)
                          ", then the PR below the separator")))
         (buffer (godel-compose
                  (plist-get task :repo) 'ship (concat title "   (drafting…)")
                  (lambda (text)
                    (pcase-let ((`(,commit ,title ,body) (godel-ship-split text)))
                      (godel-ship-execute (godel-task-get id) commit title body)))))
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
  "Ask gh about every open task PR; move merged ones to the `merged' state."
  (interactive)
  (dolist (task (godel-task-all))
    (when (and (plist-get task :pr-url) (not (eq (plist-get task :state) 'merged))
               (executable-find godel-gh-program))
      (let ((id (plist-get task :id)))
        (godel-models--run
         godel-gh-program (list "pr" "view" (plist-get task :pr-url) "--json" "state")
         (lambda (_exit out)
           (let ((state (ignore-errors
                          (alist-get 'state (json-parse-string (string-trim out) :object-type 'alist)))))
             (when (and (equal state "MERGED") (godel-task-get id))
               (godel-task-update id :state 'merged :last-event "PR merged")))))))))

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
