;;; godel-test.el --- Tests for godel -*- lexical-binding: t -*-

;;; Commentary:
;; Run with test/run-tests.sh.  Uses temporary git repos; never touches
;; ~/.godel or a real Claude session.

;;; Code:

(require 'ert)
(require 'godel)

(defun godel-test-free-port ()
  "A local TCP port nothing is listening on."
  (let* ((server (make-network-process :name "godel-test-port" :server t :host "127.0.0.1"
                                       :service t :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defmacro godel-test-with-env (&rest body)
  "Run BODY with godel's state and files in a temporary directory."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "godel-test" t))
          (godel-directory (file-name-as-directory (expand-file-name "home" tmp)))
          (godel-worktree-root (file-name-as-directory (expand-file-name "worktrees" tmp)))
          (godel-task--tasks (make-hash-table :test #'equal))
          (godel-task--loaded-repos nil)
          (godel-event-functions nil)
          (godel-status-session-start-functions nil)
          (godel-models--cache nil)
          ;; Never reach a real notdiamond proxy from the tests.
          (godel-proxy-address (cons "127.0.0.1" (godel-test-free-port))))
     (unwind-protect (progn ,@body)
       (delete-directory tmp t))))

(defun godel-test-repo (dir)
  "Create a git repo with one commit on branch main in DIR; return it."
  (make-directory dir t)
  (let ((default-directory (file-name-as-directory dir)))
    (dolist (args '(("init" "-q" "-b" "main")
                    ("-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "--allow-empty" "-m" "init")))
      (apply #'call-process "git" nil nil nil args)))
  (file-name-as-directory dir))

(defun godel-test-wait (pred &optional secs)
  "Process events until PRED is non-nil or SECS (default 10) pass."
  (let ((deadline (+ (float-time) (or secs 10))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defmacro godel-test-with-http (status &rest body)
  "Run BODY with `godel-proxy-address' at a local server answering STATUS."
  (declare (indent 1))
  `(let* ((server (make-network-process
                   :name "godel-test-http" :server t :host "127.0.0.1" :service t :noquery t
                   :filter (lambda (conn _)
                             (process-send-string
                              conn (format "HTTP/1.1 %d X\r\nContent-Length: 2\r\n\r\n{}" ,status))
                             (delete-process conn))))
          (godel-proxy-address (cons "127.0.0.1" (process-contact server :service))))
     (unwind-protect (progn ,@body) (delete-process server))))

(defun godel-test-payload (alist)
  (base64-encode-string (encode-coding-string (json-encode alist) 'utf-8) t))

;;;; Task store

(ert-deftest godel-test-slug ()
  (should (string-match-p "\\`fix-auth-token-refresh-[0-9a-f]\\{4\\}\\'"
                          (godel-task-slug "Fix auth: token refresh!")))
  (should (string-match-p "\\`task-[0-9a-f]\\{4\\}\\'" (godel-task-slug "!!!")))
  (should (<= (length (godel-task-slug (make-string 100 ?a))) 45)))

(ert-deftest godel-test-store-roundtrip ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "Do a thing" :repo repo :base "main"
                                    :session-id "sid-1" :model "sonnet")))
      (should (equal (plist-get task :repo) repo))
      (should (equal (plist-get task :state) 'setting-up))
      (should (string-prefix-p "godel/do-a-thing-" (plist-get task :branch)))
      (should (file-exists-p (expand-file-name ".git/godel/tasks.eld" repo)))
      ;; Reload from disk.
      (clrhash godel-task--tasks)
      (setq godel-task--loaded-repos nil)
      (godel-task-load-all)
      (should (equal (plist-get (godel-task-get (plist-get task :id)) :title) "Do a thing"))
      (should (equal (plist-get (godel-task-by-session "sid-1") :id) (plist-get task :id)))
      (should (equal (plist-get (godel-task-by-directory
                                 (expand-file-name "src/deep" (plist-get task :worktree)))
                                :id)
                     (plist-get task :id)))
      (godel-task-update (plist-get task :id) :state 'working)
      (should (eq (plist-get (godel-task-get (plist-get task :id)) :state) 'working))
      (godel-task-delete (plist-get task :id))
      (should-not (godel-task-get (plist-get task :id))))))

(ert-deftest godel-test-repo-root-from-worktree ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (wt (expand-file-name "wt" tmp)))
      (let ((default-directory repo))
        (call-process "git" nil nil nil "worktree" "add" "-q" "-b" "x" wt))
      (should (equal (godel-task-repo-root wt) repo))
      (should (equal (godel-task-repo-root repo) repo)))))

;;;; Models and launch environment

(defconst godel-test-router
  '(:value "notdiamond[1m]" :name "Not Diamond" :resolved "notdiamond[1m]"
    :efforts ("low" "high")))
(defconst godel-test-sonnet
  '(:value "sonnet" :name "Sonnet" :resolved "claude-sonnet-5-5" :efforts ("low" "high")))

(defconst godel-test-inherited
  '("NOTDIAMOND_RESEARCH_MODEL_PIN=opus" "NOTDIAMOND_RESEARCH_EFFORT_PIN=max"
    "NOTDIAMOND_ROUTING=off" "ANTHROPIC_API_KEY=sk-test-not-a-real-key"
    "PATH=/usr/bin:/bin" "HOME=/tmp"))

(defun godel-test-env-value (env var)
  "Value of VAR in ENV as a child process would see it (first entry wins)."
  (cl-loop for entry in env
           when (equal entry var) return nil
           when (string-prefix-p (concat var "=") entry)
           return (substring entry (1+ (length var)))))

(ert-deftest godel-test-router-detection ()
  (should (godel-models-router-p godel-test-router))
  (should-not (godel-models-router-p godel-test-sonnet))
  (should (godel-models-router-p (godel-models-find "notdiamond[1m]"))))

(ert-deftest godel-test-launch-env-router-strips-inherited-pins ()
  (let ((launch (godel-models-launch-env godel-test-router "high" godel-test-inherited)))
    (dolist (var godel-scrubbed-variables)
      (should-not (godel-test-env-value (plist-get launch :env) var))
      (should-not (cl-some (lambda (e) (string-prefix-p (concat var "=") e))
                           (plist-get launch :env))))
    ;; The router needs no --model: the wrapper starts it there.
    (should (equal (plist-get launch :args) '("--effort" "high")))
    (should (equal (plist-get launch :expect) "notdiamond[1m]"))
    (should (equal (godel-test-env-value (plist-get launch :env) "PATH") "/usr/bin:/bin"))))

(ert-deftest godel-test-launch-env-names-the-model ()
  "Any other model is chosen with --model; no pin variable is set or inherited."
  (let* ((launch (godel-models-launch-env godel-test-sonnet "high" godel-test-inherited))
         (env (plist-get launch :env)))
    (should (equal (plist-get launch :args) '("--model" "sonnet" "--effort" "high")))
    (should (equal (plist-get launch :expect) "claude-sonnet-5-5"))
    ;; The wrapper drops --model when a pin is set, so none may get through:
    ;; neither godel's own nor one Emacs inherited (godel-test-inherited has all).
    (dolist (var godel-scrubbed-variables)
      (should-not (godel-test-env-value env var))
      (should-not (cl-some (lambda (e) (string-prefix-p (concat var "=") e)) env))))
  ;; Without an effort there is no --effort, but still the model.
  (should (equal (plist-get (godel-models-launch-env godel-test-sonnet nil godel-test-inherited)
                            :args)
                 '("--model" "sonnet")))
  ;; A malformed effort fails here, before anything is started, for every model.
  (dolist (entry (list godel-test-router godel-test-sonnet))
    (should-error (godel-models-launch-env entry 5 godel-test-inherited)
                  :type 'wrong-type-argument)))

(ert-deftest godel-test-default-effort-is-medium ()
  (should (equal (eval (car (get 'godel-default-effort 'standard-value))) "medium")))

(ert-deftest godel-test-launch-env-router-effort ()
  "Not Diamond gets the effort as --effort only, never as the effort pin."
  (dolist (effort '("low" "medium" "max"))
    (let ((launch (godel-models-launch-env godel-test-router effort godel-test-inherited)))
      (should (equal (plist-get launch :args) (list "--effort" effort)))
      (should-not (godel-test-env-value (plist-get launch :env) "NOTDIAMOND_RESEARCH_EFFORT_PIN"))
      (should-not (cl-some (lambda (e) (string-prefix-p "NOTDIAMOND_RESEARCH_EFFORT_PIN=" e))
                           (plist-get launch :env)))))
  ;; No effort, no flag.
  (should (null (plist-get (godel-models-launch-env godel-test-router nil godel-test-inherited)
                           :args))))

(defun godel-test-child-env (env)
  "Environment lines a real child process sees with ENV."
  (let ((process-environment env)
        (default-directory temporary-file-directory))
    (with-temp-buffer
      (call-process "env" nil t nil)
      (split-string (buffer-string) "\n" t))))

(defun godel-test-leaks (lines &optional names)
  "The LINES (NAME=VALUE) that give one of NAMES a value.
NAMES defaults to `godel-scrubbed-variables'.  Only these are checked, not every
NOTDIAMOND_ variable: the tests run wherever you are, often inside a
notdiamond session whose own variables (NOTDIAMOND_CLAUDE_SESSION_ID, ...)
the wrapper resets on each launch anyway.  Returning just the offending lines
also keeps a failure from printing the whole environment, keys and all."
  (let ((names (or names godel-scrubbed-variables)))
    (seq-filter (lambda (line)
                  (cl-some (lambda (name) (string-prefix-p (concat name "=") line)) names))
                lines)))

(ert-deftest godel-test-launch-env-child-process ()
  "A real child process sees no pin and no API key, whatever the model.
That holds even if Emacs has them: a pin would make the wrapper ignore --model,
and a headless launch would use the key without asking and bypass the
forwarded login."
  (let ((inherited (append godel-test-inherited process-environment)))
    (dolist (entry (list godel-test-router godel-test-sonnet))
      (let ((child (godel-test-child-env
                    (plist-get (godel-models-launch-env entry "high" inherited) :env))))
        (should-not (godel-test-leaks child))))))

(ert-deftest godel-test-parse-initialize ()
  (let* ((output (concat
                  "Reusing running notdiamond proxy (pid 1) at http://127.0.0.1:8082\n"
                  "Launching Claude Code from /x/claude...\n"
                  "{\"type\":\"system\",\"subtype\":\"hook_started\"}\n"
                  (json-encode
                   '((type . "control_response")
                     (response . ((subtype . "success") (request_id . "godel-1")
                                  (response . ((models . [((value . "sonnet") (displayName . "Sonnet")
                                                           (description . "Sonnet 5.5")
                                                           (supportedEffortLevels . ["low" "high"])
                                                           (resolvedModel . "claude-sonnet-5-5"))
                                                          ((value . "notdiamond[1m]")
                                                           (displayName . "Not Diamond")
                                                           (description . "Routes")
                                                           (resolvedModel . "notdiamond[1m]"))])))))))
                  "\n"))
         (models (godel-models-parse-initialize output)))
    (should (= (length models) 2))
    (should (equal (plist-get (car models) :value) "sonnet"))
    (should (equal (plist-get (car models) :efforts) '("low" "high")))
    (should (godel-models-router-p (cadr models)))
    (should-not (godel-models-parse-initialize "no json here\n{\"type\":\"system\"}\n"))))

;;;; Agent flags

(ert-deftest godel-test-agent-flags ()
  (let* ((task '(:session-id "abc" :permission-mode "plan"))
         (flags (split-string-shell-command
                 (godel-agent-flags task (list :args '("--effort" "high")))))
         (settings (json-parse-string (cadr (member "--settings" flags)) :object-type 'alist)))
    (should (equal (cadr (member "--session-id" flags)) "abc"))
    (should (equal (cadr (member "--effort" flags)) "high"))
    (should (equal (cadr (member "--permission-mode" flags)) "plan"))
    (should-not (member "--model" flags))
    (dolist (e godel-agent-hook-events)
      (should (assq (intern e) (alist-get 'hooks settings))))
    (let ((resume (split-string-shell-command (godel-agent-flags task '(:args nil) t))))
      (should (equal (cadr (member "--resume" resume)) "abc"))
      (should-not (member "--session-id" resume)))))

(ert-deftest godel-test-agent-flags-resume-names-the-model ()
  "A resumed session is always started with the task's --model, exactly once.
The wrapper adds none to a Not Diamond resume, so godel passes it; without it
`godel-agent-check-launch' has no --model to find, and a task switched from
another model to Not Diamond would come back on the old one.  Any other model
already has its --model in the launch args, which must not be doubled."
  (cl-flet ((models (task entry resume)
              (let ((flags (split-string-shell-command
                            (godel-agent-flags task (godel-models-launch-env entry "low") resume))))
                (cl-loop for (a b) on flags when (equal a "--model") collect b))))
    (let ((router-task '(:session-id "abc" :model "notdiamond[1m]"))
          (sonnet-task '(:session-id "abc" :model "sonnet")))
      ;; Not Diamond: explicit on a resume, the wrapper's job when fresh.
      (should (equal (models router-task godel-test-router t) '("notdiamond[1m]")))
      (should (null (models router-task godel-test-router nil)))
      ;; Anything else: once, fresh or resumed.
      (should (equal (models sonnet-task godel-test-sonnet nil) '("sonnet")))
      (should (equal (models sonnet-task godel-test-sonnet t) '("sonnet"))))))

(ert-deftest godel-test-buffer-name ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "myrepo" tmp)))
           (task (godel-task-create :title "x" :repo repo :base "main")))
      (should (equal (godel-agent-buffer-name (plist-get task :worktree))
                     (format "*claude-code[myrepo/%s]*" (plist-get task :slug))))
      (should (equal (godel-agent-buffer-name "/some/other/dir/") "*claude-code[dir]*"))
      (should (equal (godel-agent-buffer-name "/some/other/dir/" "rev") "*claude-code[dir:rev]*")))))

;;;; Status

(ert-deftest godel-test-status-events ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "t" :repo repo :base "main" :session-id "s1"
                                    :expect-model "notdiamond[1m]" :pending-prompt "hi"))
           (id (plist-get task :id))
           (started nil))
      (add-hook 'godel-status-session-start-functions (lambda (_ ok) (push ok started)))
      (let ((default-directory repo))
        (call-process "git" nil nil nil "worktree" "add" "-q" "-b" (plist-get task :branch)
                      (directory-file-name (plist-get task :worktree)) "main"))
      (godel-status-event "SessionStart" (godel-test-payload
                                          '((session_id . "s1") (model . "notdiamond[1m]")
                                            (transcript_path . "/tmp/t.jsonl"))))
      (should (equal started '(t)))
      (should (eq (plist-get (godel-task-get id) :state) 'setting-up))
      (godel-status-event "UserPromptSubmit" (godel-test-payload '((session_id . "s1") (prompt . "hi"))))
      (should (eq (plist-get (godel-task-get id) :state) 'working))
      (godel-status-event "PreToolUse" (godel-test-payload
                                        '((session_id . "s1") (tool_name . "Bash")
                                          (tool_input . ((command . "npm test"))))))
      (should (equal (plist-get (godel-task-get id) :last-event) "Bash(npm test)"))
      (godel-status-event "Notification" (godel-test-payload
                                          '((session_id . "s1")
                                            (notification_type . "permission_prompt")
                                            (message . "Claude needs your permission"))))
      (should (eq (plist-get (godel-task-get id) :state) 'needs-attention))
      (should (equal (plist-get (godel-task-get id) :last-event) "permission: Bash(npm test)"))
      ;; Stop with no changes -> idle.
      (godel-status-event "Stop" (godel-test-payload '((session_id . "s1") (last_assistant_message . "Done"))))
      (should (godel-test-wait (lambda () (eq (plist-get (godel-task-get id) :state) 'idle))))
      ;; A new file -> needs-review with a diffstat.
      (with-temp-file (expand-file-name "new.txt" (plist-get task :worktree)) (insert "x\n"))
      (godel-status-event "Stop" (godel-test-payload '((session_id . "s1") (last_assistant_message . "Added"))))
      (should (godel-test-wait (lambda () (eq (plist-get (godel-task-get id) :state) 'needs-review))))
      (should (string-match-p "1 untracked" (plist-get (godel-task-get id) :diffstat)))
      ;; Found by cwd when the session ID is unknown.
      (godel-status-event "SessionEnd" (godel-test-payload
                                        `((session_id . "other")
                                          (cwd . ,(plist-get task :worktree)) (reason . "exit"))))
      (should (eq (plist-get (godel-task-get id) :state) 'stopped)))))

(ert-deftest godel-test-status-stale-stop-ignored ()
  "A Stop result that arrives after a newer event must not win."
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "t" :repo repo :base "main" :session-id "s2"))
           (id (plist-get task :id)))
      (let ((default-directory repo))
        (call-process "git" nil nil nil "worktree" "add" "-q" "-b" (plist-get task :branch)
                      (directory-file-name (plist-get task :worktree)) "main"))
      (godel-status-event "Stop" (godel-test-payload '((session_id . "s2"))))
      (godel-status-event "UserPromptSubmit" (godel-test-payload '((session_id . "s2") (prompt . "more"))))
      (godel-test-wait (lambda () nil) 1)
      (should (eq (plist-get (godel-task-get id) :state) 'working)))))

(ert-deftest godel-test-status-model-mismatch ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "t" :repo repo :base "main" :session-id "s3"
                                    :expect-model "notdiamond[1m]"))
           (oks nil))
      (add-hook 'godel-status-session-start-functions (lambda (_ ok) (push ok oks)))
      (godel-status-event "SessionStart" (godel-test-payload '((session_id . "s3") (model . "claude-opus-5-5"))))
      (should (equal oks '(nil)))
      (should (eq (plist-get (godel-task-get (plist-get task :id)) :state) 'model-mismatch))
      ;; SessionEnd after a mismatch keeps the mismatch visible.
      (godel-status-event "SessionEnd" (godel-test-payload '((session_id . "s3"))))
      (should (eq (plist-get (godel-task-get (plist-get task :id)) :state) 'model-mismatch)))))

(ert-deftest godel-test-status-idle-reminder-ignored ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "t" :repo repo :base "main" :session-id "s4"))
           (id (plist-get task :id)))
      (godel-task-update id :state 'idle)
      (godel-status-event "Notification" (godel-test-payload
                                          '((session_id . "s4") (notification_type . "idle_prompt")
                                            (message . "Claude is waiting for your input"))))
      (should (eq (plist-get (godel-task-get id) :state) 'idle))
      ;; Same message without a type is ignored too.
      (godel-status-event "Notification" (godel-test-payload
                                          '((session_id . "s4") (message . "Claude is waiting for your input"))))
      (should (eq (plist-get (godel-task-get id) :state) 'idle))
      ;; A question or permission request still needs attention.
      (godel-status-event "Notification" (godel-test-payload
                                          '((session_id . "s4") (notification_type . "elicitation_dialog")
                                            (message . "Claude has a question"))))
      (should (eq (plist-get (godel-task-get id) :state) 'needs-attention)))))

(ert-deftest godel-test-compose-ignores-markdown-hook ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (markdown-mode-hook (list (lambda () (error "Cannot show images"))))
           (auto-mode-alist (cons '("\\.md\\'" . markdown-mode) auto-mode-alist))
           (godel-compose--last-cancelled nil)
           (buffer (godel-compose repo 'task "test" #'ignore)))
      (should (buffer-live-p buffer))
      (with-current-buffer buffer
        (should (derived-mode-p 'godel-compose-mode))
        (should godel-compose-keys-mode)
        (godel-compose--close nil)))))

(ert-deftest godel-test-status-resume-marks-check-pending ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "t" :repo repo :base "main" :session-id "s5"
                                    :expect-model "notdiamond[1m]"))
           (oks nil))
      (add-hook 'godel-status-session-start-functions (lambda (_ ok) (push ok oks)))
      (godel-status-event "SessionStart" (godel-test-payload '((session_id . "s5") (source . "resume"))))
      (should (equal oks '(t)))
      (should (plist-get (godel-task-get (plist-get task :id)) :model-check-pending)))))

(ert-deftest godel-test-check-launch ()
  (let ((wrapper '(("node" "notdiamond" "claude") . ("PATH=/bin")))
        (claude-router '(("claude" "--settings" "{}" "--model" "notdiamond[1m]") . ("PATH=/bin")))
        (claude-sonnet '(("claude" "--model" "sonnet" "--effort" "low") . ("PATH=/bin"))))
    (should (car (godel-agent-check-launch (list wrapper claude-router) godel-test-router)))
    (should (car (godel-agent-check-launch (list wrapper claude-sonnet) godel-test-sonnet)))
    ;; The wrong model either way round.
    (should-not (car (godel-agent-check-launch (list wrapper claude-sonnet) godel-test-router)))
    (should-not (car (godel-agent-check-launch (list wrapper claude-router) godel-test-sonnet)))
    ;; No --model at all.
    (should-not (car (godel-agent-check-launch (list wrapper) godel-test-router)))
    (should-not (car (godel-agent-check-launch (list wrapper) godel-test-sonnet)))
    ;; A pin anywhere in the tree fails any session, even one whose --model is
    ;; right: with a pin the wrapper would have dropped godel's --model.
    (dolist (pin '("NOTDIAMOND_RESEARCH_MODEL_PIN=opus" "NOTDIAMOND_RESEARCH_EFFORT_PIN=max"
                   "NOTDIAMOND_ROUTING=off"))
      (let ((pinned (cons (car wrapper) (list pin))))
        (should-not (car (godel-agent-check-launch (list pinned claude-router) godel-test-router)))
        (should-not (car (godel-agent-check-launch (list pinned claude-sonnet) godel-test-sonnet)))))
    (should (string-match-p "pin variable" (cdr (godel-agent-check-launch
                                                  (list (cons (car wrapper) '("NOTDIAMOND_ROUTING=off"))
                                                        claude-sonnet)
                                                  godel-test-sonnet))))))

(ert-deftest godel-test-same-model ()
  (should (godel-status-same-model-p "claude-sonnet-5-5[1m]" "claude-sonnet-5-5"))
  (should (godel-status-same-model-p "notdiamond[1m]" "notdiamond[1m]"))
  (should-not (godel-status-same-model-p "claude-opus-5-5" "notdiamond[1m]")))

(ert-deftest godel-test-proxy-guard-starts-clean-proxy ()
  "No proxy running: godel starts one, and the pin variables and API key don't reach it."
  (godel-test-with-env
    (let* ((state (expand-file-name "proxy-started" tmp))
           (env-log (expand-file-name "start-env" tmp))
           (fake (expand-file-name "notdiamond" tmp))
           (godel-notdiamond-program fake)
           ;; Nothing listening, so the direct check fails and the CLI is used.
           (godel-proxy-address (cons "127.0.0.1" (godel-test-free-port)))
           (process-environment (append '("NOTDIAMOND_ROUTING=off"
                                          "NOTDIAMOND_RESEARCH_MODEL_PIN=opus"
                                          "ANTHROPIC_API_KEY=sk-test-not-a-real-key")
                                        process-environment))
           (result 'pending))
      (with-temp-file fake
        (insert "#!/bin/sh\n"
                "case \"$1\" in\n"
                "  status) if [ -f " (shell-quote-argument state) " ]; then echo '{\"ok\":true}'; else echo '{\"ok\":false}'; fi ;;\n"
                "  start) env > " (shell-quote-argument env-log) "; touch " (shell-quote-argument state) " ;;\n"
                "esac\n"))
      (set-file-modes fake #o755)
      (godel-models-ensure-proxy (lambda (ok &optional _) (setq result ok)))
      (should (godel-test-wait (lambda () (not (eq result 'pending)))))
      (should (eq result t))
      (should (file-exists-p state))
      (with-temp-buffer
        (insert-file-contents env-log)
        (should-not (godel-test-leaks (split-string (buffer-string) "\n" t))))
      ;; Proxy already running: no second start.
      (delete-file env-log)
      (setq result 'pending)
      (godel-models-ensure-proxy (lambda (ok &optional _) (setq result ok)))
      (should (godel-test-wait (lambda () (not (eq result 'pending)))))
      (should (eq result t))
      (should-not (file-exists-p env-log)))))

(ert-deftest godel-test-proxy-check-direct ()
  "The proxy is asked directly; the CLI only runs if that fails."
  (godel-test-with-env
    (let* ((log (expand-file-name "cli-calls" tmp))
           (fake (expand-file-name "notdiamond" tmp))
           (godel-notdiamond-program fake)
           (result 'pending))
      (with-temp-file fake
        (insert "#!/bin/sh\necho \"$1\" >> " (shell-quote-argument log) "\necho '{\"ok\":false}'\n"))
      (set-file-modes fake #o755)
      ;; 200: up, no CLI.
      (godel-test-with-http 200
        (godel-models--proxy-up-p (lambda (up) (setq result up)))
        (should (godel-test-wait (lambda () (not (eq result 'pending))) 3))
        (should (eq result t))
        (should-not (file-exists-p log)))
      ;; 500: falls back to the CLI (which says not ok here).
      (setq result 'pending)
      (godel-test-with-http 500
        (godel-models--proxy-up-p (lambda (up) (setq result up)))
        (should (godel-test-wait (lambda () (not (eq result 'pending))) 5))
        (should (null result))
        (should (file-exists-p log)))
      ;; Nothing listening: falls back to the CLI too.
      (delete-file log)
      (setq result 'pending)
      (let ((godel-proxy-address (cons "127.0.0.1" (godel-test-free-port))))
        (godel-models--proxy-up-p (lambda (up) (setq result up)))
        (should (godel-test-wait (lambda () (not (eq result 'pending))) 5))
        (should (null result))
        (should (file-exists-p log))))))

(ert-deftest godel-test-proxy-address-from-config ()
  (godel-test-with-env
    (let ((config (expand-file-name "nd.json" tmp))
          (godel-proxy-address nil))
      (with-temp-file config (insert "{\"server\":{\"port\":9123,\"host\":\"127.0.0.2\"}}"))
      (let ((process-environment (cons (concat "NOTDIAMOND_CONFIG=" config) process-environment)))
        (should (equal (godel-models--proxy-address) '("127.0.0.2" . 9123))))
      (let ((process-environment (cons (concat "NOTDIAMOND_CONFIG=" tmp "/missing.json")
                                       process-environment)))
        (should (equal (godel-models--proxy-address) '("127.0.0.1" . 8082)))))))

(ert-deftest godel-test-workspace-badge ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "badge me" :repo repo :base "main"))
           (slug (plist-get task :slug)))
      (godel-task-update (plist-get task :id) :state 'needs-attention)
      (should (equal (godel-workspace-badge slug) "●"))
      (godel-task-update (plist-get task :id) :state 'needs-review)
      (should (equal (godel-workspace-badge slug) "✓"))
      (godel-task-update (plist-get task :id) :state 'idle)
      (should (equal (godel-workspace-badge slug) ""))
      (should (equal (godel-workspace-badge "main") "")))))

(defun godel-test-board-tasks (tmp)
  "Two repos (zeta, alpha) with tasks in several states; returns the tasks."
  (let* ((zeta (godel-test-repo (expand-file-name "zeta" tmp)))
         (alpha (godel-test-repo (expand-file-name "alpha" tmp)))
         (a1 (godel-task-create :title "Alpha working" :repo alpha :base "main"))
         (a2 (godel-task-create :title "Alpha needs me" :repo alpha :base "main"))
         (z1 (godel-task-create :title "Zeta review" :repo zeta :base "main")))
    (godel-task-update (plist-get a1 :id) :state 'working :last-event "Edit(x.el)")
    (godel-task-update (plist-get a2 :id) :state 'needs-attention
                       :last-event "permission: Bash(npm test)")
    (godel-task-update (plist-get z1 :id) :state 'needs-review)
    (list a1 a2 z1)))

(defun godel-test-text-order-p (&rest strings)
  "Non-nil if STRINGS appear in the current buffer in this order."
  (goto-char (point-min))
  (cl-every (lambda (s) (search-forward s nil t)) strings))

(ert-deftest godel-test-board-groups-by-repo ()
  (godel-test-with-env
    (godel-test-board-tasks tmp)
    (with-current-buffer (godel-board-buffer)
      (unwind-protect
          (progn
            (should (godel-test-text-order-p "2 repos" "alpha" "Needs attention (1)" "Alpha needs me"
                                             "Working (1)" "Alpha working"
                                             "zeta" "Needs review (1)" "Zeta review"))
            ;; Repo heading counts; no repo column in task rows.
            (should (godel-test-text-order-p "alpha" "●1 ◐1"))
            (goto-char (point-min))
            (search-forward "Alpha working")
            (should-not (let ((case-fold-search nil))
                          (string-match-p "alpha" (buffer-substring (line-beginning-position)
                                                                    (line-end-position)))))
            ;; n on a repo heading uses that repo.
            (goto-char (point-min))
            (search-forward "zeta")
            (should (string-suffix-p "/zeta/" (godel-board--repo-at-point)))
            ;; A collapsed repo stays collapsed across refreshes.
            (magit-section-hide (magit-current-section))
            (godel-board-refresh)
            (let (hidden)
              (magit-map-sections (lambda (sec) (when (eq (oref sec type) 'godel-repo)
                                                  (push (cons (file-name-nondirectory
                                                               (directory-file-name (oref sec value)))
                                                              (oref sec hidden))
                                                        hidden))))
              (should (equal (cdr (assoc "zeta" hidden)) t))
              (should (equal (cdr (assoc "alpha" hidden)) nil))))
        (kill-buffer)))))

(defun godel-test-line-with (text)
  "The line of the current buffer containing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (buffer-substring (line-beginning-position) (line-end-position)))

(ert-deftest godel-test-board-shows-effort ()
  "Board and sidebar show the effort for every model, Not Diamond included."
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (nd (godel-task-create :title "Routed default" :repo repo :base "main"
                                  :model "notdiamond[1m]"))
           (nd-x (godel-task-create :title "Routed xhigh" :repo repo :base "main"
                                    :model "notdiamond[1m]" :effort "xhigh"))
           (son (godel-task-create :title "Pinned sonnet" :repo repo :base "main"
                                   :model "sonnet" :effort "low")))
      ;; No effort on the task: the default, which its launch would use.
      (should (equal (godel-board--model-label nd) "Not Diamond/medium"))
      (should (equal (godel-board--model-label nd-x) "Not Diamond/xhigh"))
      (should (equal (godel-board--model-label son) "Sonnet/low"))
      (should (equal (godel-board--model-label '(:model "gone")) "gone"))
      ;; Full board: in the task's row, not cut short.
      (with-current-buffer (godel-board-buffer)
        (unwind-protect
            (progn
              (should (string-match-p "Not Diamond/medium " (godel-test-line-with "Routed default")))
              (should (string-match-p "Not Diamond/xhigh " (godel-test-line-with "Routed xhigh")))
              (should (string-match-p "Sonnet/low " (godel-test-line-with "Pinned sonnet")))
              ;; And in the details.
              (should (godel-test-text-order-p "Routed xhigh" "model" "Not Diamond/xhigh")))
          (kill-buffer)))
      ;; Sidebar: in the task's details.
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () nil)))
        (with-current-buffer (godel-board-buffer t)
          (unwind-protect
              (progn
                (should (godel-test-text-order-p "Routed default" "model" "Not Diamond/medium"))
                (should (godel-test-text-order-p "Routed xhigh" "model" "Not Diamond/xhigh")))
            (kill-buffer)))))))

(ert-deftest godel-test-sidebar-compact-view ()
  (godel-test-with-env
    (let* ((tasks (godel-test-board-tasks tmp))
           (current (plist-get (nth 0 tasks) :slug)))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () current)))
        (with-current-buffer (godel-board-buffer t)
          (unwind-protect
              (progn
                (should-not (godel-test-text-order-p "Needs attention"))
                ;; Within alpha, state order: the attention task comes first.
                (should (godel-test-text-order-p "alpha" "Alpha needs me" "permission: Bash"
                                                 "Alpha working" "zeta" "Zeta review"))
                (goto-char (point-min))
                (search-forward "Alpha working")
                (should (string-prefix-p "▶" (buffer-substring (line-beginning-position)
                                                               (line-end-position))))
                (goto-char (point-min))
                (search-forward "Zeta review")
                (should-not (string-prefix-p "▶" (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
                ;; Room for magit's "…" after a collapsed row.
                (should (<= (string-width (buffer-substring (line-beginning-position)
                                                            (line-end-position)))
                            (- godel-sidebar-width 3))))
            (kill-buffer)))))))

(ert-deftest godel-test-sidebar-toggle-and-sync ()
  (godel-test-with-env
    (let* ((tasks (godel-test-board-tasks tmp))
           (workspace "main")
           (godel-sidebar-hidden nil)
           (godel-sidebar-auto-open t))
      (cl-letf (((symbol-function '+workspace-current-name) (lambda () workspace)))
        (unwind-protect
            (progn
              (delete-other-windows)
              ;; Not a task workspace: sync doesn't open it.
              (godel-sidebar-sync)
              (godel-test-wait (lambda () nil) 0.2)
              (should-not (godel-sidebar-window))
              ;; Toggle opens it anywhere; it's a left side window.
              (godel-sidebar-toggle)
              (should (godel-sidebar-window))
              (should (eq (window-parameter (godel-sidebar-window) 'window-side) 'left))
              ;; delete-other-windows keeps it.
              (with-selected-window (window-main-window) (delete-other-windows))
              (should (godel-sidebar-window))
              ;; Toggle hides it, and it stays hidden after switching to a task.
              (godel-sidebar-toggle)
              (should-not (godel-sidebar-window))
              (should godel-sidebar-hidden)
              (setq workspace (plist-get (nth 0 tasks) :slug))
              (godel-sidebar-sync)
              (godel-test-wait (lambda () nil) 0.2)
              (should-not (godel-sidebar-window))
              ;; Toggled back on: a task workspace re-attaches it after a switch
              ;; that dropped it.
              (godel-sidebar-toggle)
              (should-not godel-sidebar-hidden)
              (delete-window (godel-sidebar-window))
              (godel-sidebar-sync)
              (should (godel-test-wait #'godel-sidebar-window 1)))
          (godel-sidebar--remove)
          (when (get-buffer godel-sidebar-buffer-name) (kill-buffer godel-sidebar-buffer-name)))))))

(ert-deftest godel-test-board-colours-survive-font-lock ()
  "Icons keep their state colour after font-lock fontifies the board."
  (godel-test-with-env
    (godel-test-board-tasks tmp)
    (with-current-buffer (godel-board-buffer)
      (unwind-protect
          (progn
            (font-lock-mode 1)
            (font-lock-ensure)
            (dolist (case '(("●" . godel-state-needs-attention)
                            ("◐" . godel-state-working)
                            ("✓" . godel-state-needs-review)))
              (goto-char (point-min))
              ;; Skip the counts in the repo heading; find the task row's icon.
              (re-search-forward (concat "^ +" (car case) " "))
              (goto-char (match-beginning 0))
              (skip-chars-forward " ")
              ;; font-lock removes `face'; `font-lock-face' is what it displays.
              (let ((face (get-text-property (point) 'font-lock-face)))
                (should (eq (if (consp face) (car face) face) (cdr case))))))
        (kill-buffer)))))

(ert-deftest godel-test-state-faces-distinct ()
  (let ((faces (mapcar (lambda (st) (let ((f (godel-board--state-face st))) (if (consp f) (car f) f)))
                       godel-task-states)))
    (should (equal (length (delete-dups (copy-sequence faces))) (length godel-task-states)))
    (dolist (face faces) (should (facep face)))))

(ert-deftest godel-test-state-colour-is-foreground-only ()
  "The theme's ANSI colour is used as a foreground, never a background."
  (cl-letf (((symbol-function 'face-foreground)
             (lambda (face &rest _) (and (eq face 'ansi-color-yellow) "#ffff88"))))
    (let ((face (godel-board--state-face 'working)))
      (should (equal face '(godel-state-working (:foreground "#ffff88"))))
      (should-not (plist-get (cadr face) :background))))
  ;; No ANSI colour defined: just the face.
  (cl-letf (((symbol-function 'face-foreground) (lambda (&rest _) nil)))
    (should (eq (godel-board--state-face 'working) 'godel-state-working)))
  (should (eq (godel-board--state-face 'stopped) 'godel-state-stopped)))

(ert-deftest godel-test-board-workspace ()
  (godel-test-with-env
    (godel-test-board-tasks tmp)
    (let (switched)
      (cl-letf (((symbol-function '+workspace-switch)
                 (lambda (name &optional create) (push (list name create) switched))))
        (unwind-protect
            (progn
              (godel-board)
              (should (equal switched '(("Godel" t))))
              (should (eq (window-buffer (selected-window)) (get-buffer godel-board-buffer-name)))
              ;; Again: same workspace, board window reused.
              (godel-board)
              (should (equal (car switched) '("Godel" t)))
              (should (eq (window-buffer (selected-window)) (get-buffer godel-board-buffer-name))))
          (kill-buffer godel-board-buffer-name))))))

(ert-deftest godel-test-badge-has-state-face ()
  (godel-test-with-env
    (let* ((tasks (godel-test-board-tasks tmp))
           (badge (godel-workspace-badge (plist-get (nth 1 tasks) :slug))))
      (should (equal badge "●"))
      (let ((f1 (get-text-property 0 'face badge))
            (f2 (get-text-property 0 'face (godel-workspace-badge (plist-get (nth 0 tasks) :slug)))))
        (should (eq (if (consp f1) (car f1) f1) 'godel-state-needs-attention))
        (should (eq (if (consp f2) (car f2) f2) 'godel-state-working))))))

(defun godel-test-started-task (tmp state)
  "A task in TMP whose session has started before, now in STATE."
  (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
         (task (godel-task-create :title "r" :repo repo :base "main" :model "notdiamond[1m]"
                                  :session-id "sid-r")))
    (make-directory (plist-get task :worktree) t)
    (godel-task-update (plist-get task :id) :state state
                       :transcript (expand-file-name "t.jsonl" tmp))))

(ert-deftest godel-test-resumable ()
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'needs-review))
           (id (plist-get task :id)))
      (should (godel-agent-resumable-p task))
      (dolist (state '(model-mismatch setting-up))
        (godel-task-update id :state state)
        (should-not (godel-agent-resumable-p task)))
      (godel-task-update id :state 'idle :transcript nil)
      (should-not (godel-agent-resumable-p task))
      (godel-task-update id :transcript (expand-file-name "t.jsonl" tmp))
      (should (godel-agent-resumable-p task))
      (cl-letf (((symbol-function 'godel-agent-live-p) (lambda (_) t)))
        (should-not (godel-agent-resumable-p task))))))

(ert-deftest godel-test-ensure-session ()
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'idle))
           (id (plist-get task :id))
           (fake (generate-new-buffer " fake-session"))
           (started nil) (got 'pending))
      (unwind-protect
          (cl-letf (((symbol-function 'godel-agent-start)
                     (lambda (task resume on-launch)
                       (setq started (list (plist-get task :id) resume))
                       (funcall on-launch fake)))
                    ((symbol-function 'godel-agent-buffer) (lambda (_) fake)))
            ;; Resumable: resumes, and calls back with the buffer after launch.
            (godel-agent-ensure-session task (lambda (b) (setq got b)))
            (should (equal started (list id t)))
            (should (eq got fake))
            ;; Running: straight away, no new start.
            (setq started nil got 'pending)
            (cl-letf (((symbol-function 'godel-agent-live-p) (lambda (_) t)))
              (godel-agent-ensure-session task (lambda (b) (setq got b))))
            (should (eq got fake))
            (should-not started)
            ;; Not resumable (wrong model): nil, no start.
            (setq got 'pending)
            (godel-task-update id :state 'model-mismatch)
            (godel-agent-ensure-session task (lambda (b) (setq got b)))
            (should (null got))
            (should-not started))
        (kill-buffer fake)))))

(ert-deftest godel-test-one-start-per-task ()
  "A second resume while one is starting doesn't launch again (the
instance-name prompt bug): it waits for the first and gets its buffer."
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'needs-review))
           (stale (copy-sequence task))
           (fake (generate-new-buffer " fake-session"))
           (launches 0) (proxy-callbacks nil) (got1 'pending) (got2 'pending))
      (unwind-protect
          (cl-letf (((symbol-function 'godel-models-ensure-proxy)
                     (lambda (cb) (push cb proxy-callbacks)))
                    ((symbol-function 'godel-agent--launch)
                     (lambda (&rest _) (setq launches (1+ launches))))
                    ((symbol-function 'godel-agent-buffer) (lambda (_) fake)))
            ;; First resume (e.g. the workspace switch) is waiting on the proxy.
            (godel-agent-ensure-session task (lambda (b) (setq got1 b)))
            (should (godel-agent-starting-p task))
            ;; Second request with the stale plist (e.g. RET): no new start.
            (should-not (godel-agent-resumable-p stale))
            (godel-agent-ensure-session stale (lambda (b) (setq got2 b)))
            (should (= (length proxy-callbacks) 1))
            ;; The proxy answers: one launch, both callers get the buffer.
            (funcall (car proxy-callbacks) t)
            (should (= launches 1))
            (should (eq got1 fake))
            (should (eq got2 fake))
            (should-not (godel-agent-starting-p task)))
        (kill-buffer fake)))))

(ert-deftest godel-test-restart-refused-while-starting ()
  "R on a task that's still starting doesn't launch a second session."
  (godel-test-with-env
    (let ((task (godel-test-started-task tmp 'idle)))
      (puthash (plist-get task :id) (list #'ignore) godel-agent--starting)
      (with-temp-buffer
        (cl-letf (((symbol-function 'godel-board--task) (lambda () task))
                  ((symbol-function 'godel-agent-start) (lambda (&rest _) (error "Must not start"))))
          (should-error (godel-board-restart) :type 'user-error)))
      (remhash (plist-get task :id) godel-agent--starting))))

(ert-deftest godel-test-restart-asks-effort-for-router ()
  "R asks for effort on Not Diamond too: the task's, else the default."
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'idle))
           (id (plist-get task :id))
           offered)
      (cl-letf (((symbol-function 'godel-board--task) (lambda () (godel-task-get id)))
                ((symbol-function 'godel-new--read-model) (lambda (&rest _) "notdiamond[1m]"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest args) (push (nth 4 args) offered) "low"))
                ((symbol-function 'godel-agent-live-p) #'ignore)
                ((symbol-function 'godel-agent-stop) #'ignore)
                ((symbol-function 'run-with-timer) #'ignore))
        (godel-board-restart)
        (should (equal (plist-get (godel-task-get id) :model) "notdiamond[1m]"))
        (should (equal (plist-get (godel-task-get id) :effort) "low"))
        (godel-board-restart))
      ;; First the default (the task had no effort), then the task's own.
      (should (equal (reverse offered) '("medium" "low"))))))

(ert-deftest godel-test-launch-router-passes-effort ()
  "A Not Diamond session starts with --effort and no pin variables."
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'idle))
           (id (plist-get task :id))
           (process-environment (append godel-test-inherited process-environment))
           flags env)
      (require 'claude-code-ide)
      (cl-letf (((symbol-function 'godel-agent--server-socket) (lambda () "/tmp/godel-test.sock"))
                ((symbol-function 'godel-agent--watch-trust) #'ignore)
                ((symbol-function 'claude-code-ide)
                 (lambda ()
                   (setq flags (split-string-shell-command claude-code-ide-cli-extra-flags)
                         env process-environment))))
        ;; No effort on the task: the default.
        (godel-agent--launch (godel-task-get id) nil)
        (should (equal (cadr (member "--effort" flags)) "medium"))
        (dolist (var godel-pin-variables)
          (should-not (godel-test-env-value env var)))
        (should (equal (plist-get (godel-task-get id) :expect-model) "notdiamond[1m]"))
        ;; Fresh: the wrapper starts Not Diamond on the router itself.
        (should-not (member "--model" flags))
        ;; The task's own effort.
        (godel-task-update id :effort "max")
        (godel-agent--launch (godel-task-get id) t)
        (should (equal (cadr (member "--effort" flags)) "max"))
        (should (member "--resume" flags))
        ;; Resumed: the wrapper adds nothing, so godel names the router.
        (should (equal (cadr (member "--model" flags)) "notdiamond[1m]"))
        (dolist (var godel-pin-variables)
          (should-not (godel-test-env-value env var)))))))

(ert-deftest godel-test-launch-fixed-model-passes-model ()
  "Any other model starts with --model and --effort, and no pin variable.
Not even one Emacs inherited: the wrapper would drop --model for it."
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'idle))
           (id (plist-get task :id))
           (process-environment (append godel-test-inherited process-environment))
           flags env)
      (require 'claude-code-ide)
      (godel-task-update id :model "sonnet" :effort "low")
      (cl-letf (((symbol-function 'godel-agent--server-socket) (lambda () "/tmp/godel-test.sock"))
                ((symbol-function 'godel-agent--watch-trust) #'ignore)
                ((symbol-function 'claude-code-ide)
                 (lambda ()
                   (setq flags (split-string-shell-command claude-code-ide-cli-extra-flags)
                         env process-environment))))
        (dolist (resume '(nil t))
          (godel-agent--launch (godel-task-get id) resume)
          ;; Once, whether fresh or resumed.
          (should (equal (cl-loop for (a b) on flags when (equal a "--model") collect b)
                         '("sonnet")))
          (should (equal (cadr (member "--effort" flags)) "low"))
          (should (eq (and (member "--resume" flags) t) resume))
          (should (equal (plist-get (godel-task-get id) :expect-model) "claude-sonnet-5-5"))
          (dolist (var godel-scrubbed-variables)
            (should-not (godel-test-env-value env var))
            (should-not (cl-some (lambda (e) (string-prefix-p (concat var "=") e)) env))))))))

(ert-deftest godel-test-no-launch-when-session-live ()
  "Even if a start gets through, it never launches over a live session."
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'idle))
           (launches 0))
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-agent--launch) (lambda (&rest _) (setq launches (1+ launches))))
                ((symbol-function 'godel-agent-live-p) (lambda (_) t)))
        (godel-agent-start task t))
      (should (= launches 0)))))

(ert-deftest godel-test-start-calls-on-launch-after-launch ()
  "ON-LAUNCH runs after the launch returns, not during it."
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'idle))
           (order nil))
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-agent--launch) (lambda (&rest _) (push 'launch order)))
                ((symbol-function 'godel-agent-live-p) #'ignore)
                ((symbol-function 'godel-agent-buffer) (lambda (_) 'buf)))
        (godel-agent-start task t (lambda (b) (push (list 'on-launch b) order))))
      (should (equal (reverse order) '(launch (on-launch buf))))
      ;; A failed proxy check calls back with nil.
      (setq order nil)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb nil "no proxy"))))
        (godel-agent-start (godel-task-get (plist-get task :id)) t (lambda (b) (push (list 'on-launch b) order))))
      (should (equal order '((on-launch nil)))))))

(ert-deftest godel-test-resume-keeps-state ()
  "A task that was ready for review is still ready for review after resuming."
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'needs-review))
           (id (plist-get task :id)))
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) #'ignore))
        (godel-agent-start task t))
      (should (eq (plist-get (godel-task-get id) :state) 'setting-up))
      (should (eq (plist-get (godel-task-get id) :resume-state) 'needs-review))
      (godel-status-event "SessionStart" (godel-test-payload '((session_id . "sid-r") (source . "resume"))))
      (should (eq (plist-get (godel-task-get id) :state) 'needs-review))
      (should-not (plist-get (godel-task-get id) :resume-state))
      ;; A stopped task comes back idle.
      (godel-task-update id :state 'stopped)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) #'ignore))
        (godel-agent-start (godel-task-get id) t))
      (godel-status-event "SessionStart" (godel-test-payload '((session_id . "sid-r"))))
      (should (eq (plist-get (godel-task-get id) :state) 'idle)))))

(defun godel-test-review-task (tmp)
  "A task with a real worktree: one tracked file changed, one new file."
  (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
         (default-directory repo))
    (with-temp-file (expand-file-name "app.py" repo)
      (insert "def a():\n    return 1\n\ndef b():\n    return 2\n"))
    (call-process "git" nil nil nil "add" "app.py")
    (call-process "git" nil nil nil "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "app")
    (let ((task (godel-task-create :title "rev" :repo repo :base "main")))
      (call-process "git" nil nil nil "worktree" "add" "-q" "-b" (plist-get task :branch)
                    (directory-file-name (plist-get task :worktree)) "main")
      (with-temp-file (expand-file-name "app.py" (plist-get task :worktree))
        (insert "def a():\n    return 1\n\ndef b():\n    return 20\n\ndef c():\n    return 3\n"))
      (with-temp-file (expand-file-name "new.txt" (plist-get task :worktree))
        (insert "brand new\n"))
      task)))

(ert-deftest godel-test-review-diff-includes-untracked ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (status-before (godel-task--git (plist-get task :worktree) "status" "--porcelain")))
      (save-window-excursion
        (godel-review-diff task)
        (let ((buffer (current-buffer)))
          (unwind-protect
              (with-current-buffer buffer
                (should (derived-mode-p 'magit-diff-mode))
                (should godel-comment-mode)
                (should godel-diff-comment-mode)
                (should (equal godel-review-task-id (plist-get task :id)))
                (goto-char (point-min))
                (should (search-forward "app.py" nil t))
                (goto-char (point-min))
                (should (search-forward "new.txt" nil t))
                (should (search-forward "brand new" nil t))
                ;; Mapping: the "+    return 20" line is new-file line 5.
                (goto-char (point-min))
                (search-forward "+    return 20")
                (should (equal (godel-review--diff-line-at (point)) '("app.py" . 5)))
                (should (= (line-number-at-pos (godel-review--diff-pos "app.py" 5))
                           (line-number-at-pos (point)))))
            (kill-buffer buffer))))
      ;; The worktree's own index is untouched: new.txt is still untracked.
      (should (equal (godel-task--git (plist-get task :worktree) "status" "--porcelain")
                     status-before))
      (should (string-match-p "^\\?\\? new\\.txt" status-before)))))

(ert-deftest godel-test-review-comments-resolve ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (file (expand-file-name "app.py" (plist-get task :worktree))))
      (godel-review-add-comment id "app.py" 4 5 "Why 20?")
      (let ((c (car (godel-review-comments (godel-task-get id)))))
        (should (equal (plist-get c :anchor) "def b():\n    return 20"))
        (should (eq (plist-get c :state) 'pending)))
      ;; Two lines inserted above: the comment follows its text.
      (with-temp-file file
        (insert "import os\nimport sys\ndef a():\n    return 1\n\ndef b():\n    return 20\n"))
      (let ((c (car (godel-review-resolve-all id))))
        (should (= (plist-get c :start) 6))
        (should (= (plist-get c :end) 7))
        (should (eq (plist-get c :state) 'pending)))
      ;; The text is gone: outdated.
      (with-temp-file file (insert "def b():\n    return 21\n"))
      (should (eq (plist-get (car (godel-review-resolve-all id)) :state) 'outdated))
      ;; Update and delete.
      (godel-review-add-comment id "new.txt" 1 1 "Rename this")
      (let ((cid (plist-get (cadr (plist-get (godel-task-get id) :comments)) :id)))
        (godel-review-update-comment id cid :text "Rename it")
        (should (equal (plist-get (cadr (plist-get (godel-task-get id) :comments)) :text) "Rename it"))
        (godel-review-delete-comment id cid)
        (should (= (length (plist-get (godel-task-get id) :comments)) 1))))))

(ert-deftest godel-test-review-comments-shown-in-diff-and-file ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id)))
      ;; The test environment clears this hook; godel-review installs it.
      (add-hook 'godel-event-functions #'godel-review--refresh-buffers)
      (godel-review-add-comment id "app.py" 5 5 "Why 20?")
      (save-window-excursion
        (godel-review-diff task)
        (let ((buffer (current-buffer)))
          (unwind-protect
              (with-current-buffer buffer
                (should (= (length godel-comment--overlays) 1))
                (let ((ov (car godel-comment--overlays)))
                  ;; Drawn before the next line, so the cursor can move past it.
                  (should (string-match-p "Why 20\\?" (overlay-get ov 'before-string)))
                  (should-not (overlay-get ov 'after-string))
                  (should (= (overlay-start ov)
                             (save-excursion (goto-char (godel-comment--line ov)) (1+ (line-end-position)))))
                  ;; Moving down from the commented line reaches the next line.
                  (goto-char (godel-comment--line ov))
                  (let ((line (line-number-at-pos)))
                    (forward-line 1)
                    (should (= (line-number-at-pos) (1+ line)))))
                ;; Adding a comment redraws the open diff.
                (godel-review-add-comment id "new.txt" 1 1 "Is this needed?")
                (should (= (length godel-comment--overlays) 2))
                ;; gn moves to the comments.
                (goto-char (point-min))
                (godel-comment-next)
                (should (looking-at ".*return 20")))
            (kill-buffer buffer))))
      ;; In a file of the worktree: shown, but no keys bound.
      (let ((buffer (find-file-noselect (expand-file-name "app.py" (plist-get task :worktree)))))
        (unwind-protect
            (with-current-buffer buffer
              (should godel-comment-mode)
              (should-not godel-diff-comment-mode)
              (should (= (length godel-comment--overlays) 1)))
          (kill-buffer buffer))))))

(ert-deftest godel-test-feedback-text ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id)))
      (godel-review-add-comment id "app.py" 4 5 "Why 20?")
      (godel-review-add-comment id "new.txt" 1 1 "Needed?")
      (let ((text (godel-review-feedback-text (godel-task-get id))))
        (should (string-match-p "^1\\. `app.py` lines 4-5:\n```\ndef b():\n    return 20\n```\nWhy 20\\?" text))
        (should (string-match-p "^2\\. `new.txt` line 1:\n```\nbrand new\n```\nNeeded\\?" text))))))

(ert-deftest godel-test-feedback-send-flow ()
  "v s: pending comments are sent as one message, then become addressed."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (sent nil))
      (add-hook 'godel-status-stop-functions #'godel-review--on-stop)
      (godel-task-update id :session-id "sid-fb")
      (godel-review-add-comment id "app.py" 5 5 "Why 20?")
      (cl-letf (((symbol-function 'godel-compose)
                 (lambda (_repo _kind _header callback &optional initial _quote)
                   (funcall callback (concat initial "Overall: looks close."))))
                ((symbol-function 'godel-agent-ensure-session)
                 (lambda (_task cb) (funcall cb (current-buffer))))
                ((symbol-function 'godel-tui-wait)
                 (lambda (_b _p _t cb) (funcall cb "screen")))
                ((symbol-function 'godel-agent-send)
                 (lambda (_task text) (setq sent text))))
        (godel-review-send (godel-task-get id)))
      (should (string-match-p "Why 20\\?" sent))
      (should (string-match-p "Overall: looks close\\." sent))
      (let ((c (car (plist-get (godel-task-get id) :comments))))
        (should (eq (plist-get c :state) 'sent))
        (should (= (plist-get c :round) 1)))
      (should (= (length (plist-get (godel-task-get id) :review-rounds)) 1))
      ;; Nothing pending now.
      (should-error (godel-review-send (godel-task-get id)) :type 'user-error)
      ;; The agent finishes its turn: addressed.
      (godel-status-event "Stop" (godel-test-payload '((session_id . "sid-fb"))))
      (should (eq (plist-get (car (plist-get (godel-task-get id) :comments)) :state) 'addressed)))))

(ert-deftest godel-test-review-list ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id)))
      (godel-review-add-comment id "app.py" 5 5 "Why 20?")
      (godel-review-add-comment id "new.txt" 1 1 "Needed?")
      (godel-review-update-comment id 2 :state 'sent)
      (save-window-excursion
        (godel-review-list (godel-task-get id))
        (unwind-protect
            (progn
              (should (godel-test-text-order-p "Pending (1)" "app.py:5" "return 20" "Why 20?"
                                               "Sent (1)" "new.txt:1"))
              (goto-char (point-min))
              (search-forward "app.py:5")
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (godel-review-list-delete))
              (should (= (length (plist-get (godel-task-get id) :comments)) 1)))
          (kill-buffer (godel-review--list-buffer-name id)))))))

(ert-deftest godel-test-ask-author ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (sent nil) (quote-shown nil))
      (should (equal (godel-review-question-text task "Why?") "Why?"))
      (should (equal (godel-review-question-text task "Why 20?" '("app.py" 5 5))
                     "About `app.py` line 5:\n```\n    return 20\n```\nWhy 20?"))
      (cl-letf (((symbol-function 'godel-compose)
                 (lambda (_repo kind _header callback &optional _initial quote)
                   (should (eq kind 'question))
                   (setq quote-shown quote)
                   (funcall callback "Why 20?")))
                ((symbol-function 'godel-agent-ensure-session)
                 (lambda (_task cb) (funcall cb (current-buffer))))
                ((symbol-function 'godel-tui-wait)
                 (lambda (_b _p _t cb) (funcall cb "screen")))
                ((symbol-function 'godel-agent-send)
                 (lambda (_task text) (setq sent text))))
        (godel-review-ask (godel-task-get id) '("app.py" 4 5)))
      (should (equal quote-shown "> def b():\n>     return 20"))
      (should (string-prefix-p "About `app.py` lines 4-5:" sent))
      (should (string-suffix-p "Why 20?" sent))
      ;; Asking doesn't touch the comments.
      (should-not (plist-get (godel-task-get id) :comments)))))

(defconst godel-test-reviewer-output
  (concat "Reusing running notdiamond proxy (pid 1) at http://127.0.0.1:8082\n"
          "Launching Claude Code from /x/claude...\n"
          (json-encode
           `((type . "result") (subtype . "success") (is_error . :json-false)
             (result . ,(concat "Two problems.\n<findings>"
                                (json-encode
                                 [((file . "app.py") (line . 5) (end_line . 5) (severity . "high")
                                   (comment . "20 contradicts the spec; return 2."))
                                  ((file . "new.txt") (line . 1) (severity . "low")
                                   (comment . "Unused file."))])
                                "</findings>"))))
          "\n"))

(ert-deftest godel-test-reviewer-parse ()
  (let ((findings (godel-reviewer-parse godel-test-reviewer-output)))
    (should (= (length findings) 2))
    (should (equal (car findings) '(:file "app.py" :start 5 :end 5 :severity "high"
                                    :text "20 contradicts the spec; return 2.")))
    ;; end_line missing: same as line.
    (should (= (plist-get (cadr findings) :end) 1)))
  (should (null (godel-reviewer-parse
                 "{\"type\":\"result\",\"is_error\":false,\"result\":\"Fine.<findings>[]</findings>\"}")))
  (should-error (godel-reviewer-parse "{\"type\":\"result\",\"is_error\":true,\"result\":\"overloaded\"}"))
  (should-error (godel-reviewer-parse "{\"type\":\"result\",\"is_error\":false,\"result\":\"no block\"}"))
  (should-error (godel-reviewer-parse "nothing here"))
  ;; Streamed output: the result line comes after other JSON lines.
  (should (= 0 (length (godel-reviewer-parse
                        (concat "{\"type\":\"system\",\"subtype\":\"init\"}\n"
                                "{\"type\":\"assistant\",\"message\":{\"content\":[]}}\n"
                                "{\"type\":\"result\",\"is_error\":false,\"result\":\"<findings>[]</findings>\"}\n"))))))

(ert-deftest godel-test-reviewer-prompt-has-task-and-diff ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp)))
      (godel-task-update (plist-get task :id) :prompt "Change b() to return 20 and add c().")
      (let ((prompt (godel-reviewer-prompt (godel-task-get (plist-get task :id)))))
        (should (string-match-p "Change b() to return 20" prompt))
        (should (string-match-p "^\\+    return 20" prompt))
        ;; Untracked files are in the diff too.
        (should (string-match-p "^\\+brand new" prompt))
        (should (string-match-p "<findings>" prompt))))))

(ert-deftest godel-test-reviewer-run ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (process-environment (append '("GODEL_SERVER=/should/not/leak"
                                          "ANTHROPIC_API_KEY=sk-test-not-a-real-key")
                                        process-environment))
           ran activity-seen badge-seen notified)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) (lambda (title body) (push (list title body) notified)))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (program args input env on-message on-exit)
                   (setq ran (list :program program :args args :input input :env env))
                   ;; A tool call while it works: shown as the review activity.
                   (funcall on-message
                            '((type . "assistant")
                              (message . ((content . (((type . "tool_use") (name . "Read")
                                                       (input . ((file_path . "/w/app.py"))))))))))
                   (setq activity-seen (plist-get (godel-task-get id) :review-activity))
                   (setq badge-seen (godel-board--review-badge (godel-task-get id)))
                   (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id)))
      ;; Streamed, read-only, headless, no session saved, no --model for Not
      ;; Diamond (only --effort), no godel hooks.
      (should (member "stream-json" (plist-get ran :args)))
      (should (member "--no-session-persistence" (plist-get ran :args)))
      (should (equal activity-seen "Read(app.py)"))
      (should (equal (car badge-seen) "⟳"))
      (should (string-match-p "\\`reviewing [0-9]+s · Read(app.py)\\'" (cadr badge-seen)))
      (should (equal notified '(("godel: review of rev is done" "2 findings (v l on the board to see them)"))))
      (should (member "Edit,Write,MultiEdit,NotebookEdit,Bash" (plist-get ran :args)))
      (should-not (member "--settings" (plist-get ran :args)))
      (should (equal (cadr (member "--effort" (plist-get ran :args))) godel-default-effort))
      ;; Pin variables and GODEL_SERVER are unset for the child (bare names in
      ;; front), even though Emacs has GODEL_SERVER set.
      (should-not (godel-test-leaks (godel-test-child-env (plist-get ran :env))
                                    (cons "GODEL_SERVER" godel-scrubbed-variables)))
      (should (equal (car (plist-get ran :env)) "GODEL_SERVER"))
      (should (string-match-p "return 20" (plist-get ran :input)))
      (let ((task (godel-task-get id)))
        (should (eq (plist-get task :review-status) 'done))
        (should (equal (plist-get task :review-note) "2 findings"))
        (should (equal (mapcar (lambda (f) (plist-get f :state)) (plist-get task :findings))
                       '(proposed proposed)))
        ;; The task's own state is untouched.
        (should (eq (plist-get task :state) 'setting-up)))
      ;; Accept one, drop the other.
      (godel-reviewer-accept id 1)
      (godel-reviewer-drop id 2)
      (let ((task (godel-task-get id)))
        (should (equal (mapcar (lambda (f) (plist-get f :state)) (plist-get task :findings))
                       '(accepted dropped)))
        (should (equal (plist-get (car (plist-get task :comments)) :text)
                       "[reviewer, high] 20 contradicts the spec; return 2."))
        (should (eq (plist-get (car (plist-get task :comments)) :state) 'pending)))
      ;; A failed run is recorded and notified, not raised.
      (setq notified nil)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) (lambda (title body) (push (list title body) notified)))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a _i _e _m on-exit) (funcall on-exit 1 "boom"))))
        (godel-reviewer-run (godel-task-get id)))
      (should (eq (plist-get (godel-task-get id) :review-status) 'failed))
      (should (equal (caar notified) "godel: review of rev failed")))))

;;;; The author's briefing for reviewers

(defun godel-test-briefing-output (text &optional error)
  "What a headless run prints: wrapper lines, then the result."
  (concat "Reusing running notdiamond proxy (pid 1) at http://127.0.0.1:8082\n"
          "Launching Claude Code from /x/claude...\n"
          (json-encode `((type . "result") (subtype . "success")
                         (is_error . ,(if error t :json-false)) (result . ,text)))
          "\n"))

(defconst godel-test-briefing
  "## Problem\nUsers got logged out.\n\n## Solution\nRefresh early; the user chose 5 min over 1 min.\n\n## Tested\nunit tests; not the UI."
  "A briefing like the author would write.")

(defun godel-test-briefed-task (tmp &rest props)
  "A review task whose author's session has started, with PROPS."
  (let* ((task (godel-test-review-task tmp))
         (id (plist-get task :id)))
    (apply #'godel-task-update id :transcript (expand-file-name "t.jsonl" tmp)
           :session-id "author-sid" props)))

(ert-deftest godel-test-parse-briefing ()
  (should (equal (godel-reviewer-parse-briefing (godel-test-briefing-output "  the briefing\n"))
                 "the briefing"))
  (should-error (godel-reviewer-parse-briefing (godel-test-briefing-output "overloaded" t))
                :type 'error)
  (should (string-match-p "overloaded"
                          (cadr (should-error (godel-reviewer-parse-briefing
                                               (godel-test-briefing-output "overloaded" t))))))
  (should-error (godel-reviewer-parse-briefing (godel-test-briefing-output "   ")))
  (should-error (godel-reviewer-parse-briefing "no json here\n")))

(ert-deftest godel-test-reviewer-prompts-carry-briefing ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (tricky "Used 100% of the budget; format %s and %d stay literal.\n## Tested\nnothing")
           (plain (godel-reviewer-prompt task))
           (briefed (godel-reviewer-prompt task tricky))
           (walk-plain (godel-reviewer-walkthrough-prompt task))
           (walk (godel-reviewer-walkthrough-prompt task tricky)))
      ;; Without a briefing: no section, and the prompt reads as before.
      (dolist (text (list plain walk-plain))
        (should-not (string-match-p "briefing" text))
        (should (string-match-p "^\\+    return 20" text)))
      (should (string-match-p "^The task they were given:\n.*\n\nTheir changes" plain))
      ;; With one: the author's words, framed as something to check, ahead of
      ;; the diff, with the diff still there.
      (dolist (text (list briefed walk))
        (should (string-match-p "<briefing>\nUsed 100% of the budget; format %s and %d stay literal\\."
                                text))
        (should (string-match-p "don't take it on trust" text))
        (should (< (string-match "<briefing>" text) (string-match "```diff" text)))
        (should (string-match-p "^\\+    return 20" text)))
      ;; Blank counts as none.
      (should-not (string-match-p "briefing" (godel-reviewer-prompt task "  \n"))))))

(ert-deftest godel-test-briefing-forks-the-authors-session ()
  "The author is asked in a read-only fork of its session, as itself."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp :model "sonnet" :effort "low"))
           (id (plist-get task :id))
           (process-environment (append godel-test-inherited process-environment))
           ran result)
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (program args callback &optional input env)
                   (setq ran (list :program program :args args :input input :env env
                                   :dir default-directory))
                   (funcall callback 0 (godel-test-briefing-output godel-test-briefing))
                   'proc)))
        (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq result r))))
      (let ((args (plist-get ran :args)))
        (should (equal (cadr (member "--resume" args)) "author-sid"))
        (should (member "--fork-session" args))
        (should (member "-p" args))
        (should (member "--no-session-persistence" args))
        (should (member "Edit,Write,MultiEdit,NotebookEdit,Bash" args))
        ;; The author's own model and effort: Sonnet at low.
        (should (equal (cadr (member "--model" args)) "sonnet"))
        (should (equal (cadr (member "--effort" args)) "low"))
        (should-not (member "--settings" args)))
      (should (equal (plist-get ran :dir) (plist-get task :worktree)))
      (should (string-match-p "## Problem" (plist-get ran :input)))
      (should (string-match-p "## Tested" (plist-get ran :input)))
      ;; Never reports as the task.
      (should (equal (car (plist-get ran :env)) "GODEL_SERVER"))
      ;; The model goes by --model; no pin variable (it would make the wrapper
      ;; drop that flag).
      (should-not (godel-test-leaks (godel-test-child-env (plist-get ran :env))
                                    godel-pin-variables))
      (should (equal result (list godel-test-briefing nil)))
      (should-not (gethash id godel-reviewer--briefing-runs)))))

(ert-deftest godel-test-briefing-skipped-or-failed ()
  (godel-test-with-env
    (let* ((fresh (godel-test-review-task tmp))   ; its session never started
           (briefed (godel-test-briefed-task tmp))
           (id (plist-get briefed :id))
           result)
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (&rest _) (error "Must not ask"))))
        ;; Nobody to ask: no transcript, or the option is off.
        (godel-reviewer-briefing (godel-task-get (plist-get fresh :id))
                                 (lambda (&rest r) (setq result r)))
        (should (equal result '(nil nil)))
        (setq result 'unset)
        (let ((godel-reviewer-ask-author nil))
          (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq result r))))
        (should (equal result '(nil nil))))
      ;; The author fails: the reason comes back, nothing is raised.
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _)
                   (funcall callback 1 (godel-test-briefing-output "overloaded" t)))))
        (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq result r))))
      (should (null (car result)))
      (should (string-match-p "overloaded" (cadr result)))
      (should-not (gethash id godel-reviewer--briefing-runs))
      ;; Claude can't even be started.
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (&rest _) (signal 'file-missing '("Searching for program" "no such file")))))
        (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq result r))))
      (should (null (car result)))
      (should (cadr result))
      (should-not (gethash id godel-reviewer--briefing-runs)))))

(ert-deftest godel-test-briefing-shared-by-concurrent-askers ()
  "A review and a walkthrough started together ask the author once and share the answer."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           (asked 0) pending walkthrough-got)
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _) (cl-incf asked) (setq pending callback) 'proc))
                ((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a _i _e _m on-exit) (funcall on-exit 0 godel-test-reviewer-output))))
        ;; First asker (the walkthrough's), then a review while it's pending.
        (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq walkthrough-got r)))
        (should (gethash id godel-reviewer--briefing-runs))
        (godel-reviewer-run (godel-task-get id))
        (should (= asked 1))
        (should (eq (plist-get (godel-task-get id) :review-status) 'running))
        ;; The answer arrives once, for both.
        (funcall pending 0 (godel-test-briefing-output godel-test-briefing))
        (should-not (gethash id godel-reviewer--briefing-runs))
        (should (equal walkthrough-got (list godel-test-briefing nil)))
        (should (eq (plist-get (godel-task-get id) :review-status) 'done))
        (should (equal (plist-get (godel-task-get id) :review-note) "2 findings (briefed by the author)"))
        ;; Free again: the next ask is a new one.
        (godel-reviewer-briefing (godel-task-get id) #'ignore)
        (should (= asked 2))))))

(ert-deftest godel-test-briefing-one-waiter-failing-doesnt-silence-the-rest ()
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           pending heard)
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _) (setq pending callback) 'proc))
                ((symbol-function 'message) #'ignore))
        (godel-reviewer-briefing (godel-task-get id) (lambda (&rest _) (error "first waiter breaks")))
        (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq heard r)))
        (funcall pending 0 (godel-test-briefing-output godel-test-briefing)))
      (should (equal heard (list godel-test-briefing nil)))
      (should-not (gethash id godel-reviewer--briefing-runs)))))

(defun godel-test-pid-running-p (pid)
  "Non-nil if process PID exists and isn't a zombie."
  (let ((state (alist-get 'state (ignore-errors (process-attributes pid)))))
    (and state (not (equal state "Z")))))

(ert-deftest godel-test-briefing-times-out ()
  "A hung author is killed after the timeout, with everything it started.
The real one is the notdiamond wrapper, a parent of the real `claude'.  Killing
only the wrapper orphans `claude', which keeps running and spending tokens
(checked against the real wrapper), so this uses a parent with a child."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           (godel-reviewer-briefing-timeout 1.0)
           proc child result)
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _)
                   ;; A parent that starts a child, tells us its pid, and never answers.
                   (setq proc (make-process
                               :name "godel-test-hung"
                               :command '("sh" "-c" "sleep 60 & echo $!; wait")
                               :noquery t :connection-type 'pipe
                               :filter (lambda (_p out)
                                         (when (and (not child) (string-match "\\([0-9]+\\)" out))
                                           (setq child (string-to-number (match-string 1 out)))))
                               :sentinel (lambda (p _)
                                           (unless (process-live-p p)
                                             (funcall callback (process-exit-status p) ""))))))))
        (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq result r)))
        (should (godel-test-wait (lambda () child) 5))
        (should (process-live-p proc))
        (should (godel-test-pid-running-p child))
        (should (godel-test-wait (lambda () result) 8)))
      ;; Both the parent and the child it started are gone.
      (should-not (process-live-p proc))
      (should (godel-test-wait (lambda () (not (godel-test-pid-running-p child))) 5))
      (should (null (car result)))
      (should (string-match-p "no answer after" (cadr result)))
      (should-not (gethash id godel-reviewer--briefing-runs)))))

(ert-deftest godel-test-answer-in-time-cancels-the-timeout ()
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           (before (length timer-list)))
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _)
                   (run-at-time 0.05 nil callback 0 (godel-test-briefing-output godel-test-briefing))
                   'proc)))
        (let (result)
          (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq result r)))
          (should (godel-test-wait (lambda () result) 5))
          (should (equal result (list godel-test-briefing nil)))))
      ;; The 300 s timer is gone, not left to fire later.
      (should (<= (length timer-list) before)))))

(ert-deftest godel-test-review-without-merge-base-fails-at-once ()
  "No merge base: the user-error comes at the key press, with nothing marked or opened."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (cl-letf (((symbol-function 'godel-review-merge-base) (lambda (&rest _) nil))
                ((symbol-function 'godel-models-ensure-proxy) (lambda (&rest _) (error "Too far")))
                ((symbol-function 'godel-reviewer--start-session) (lambda (&rest _) (error "Must not open")))
                ((symbol-function 'godel-models--run) (lambda (&rest _) (error "Must not ask"))))
        (should-error (godel-reviewer-run (godel-task-get id)) :type 'user-error)
        (should-not (plist-get (godel-task-get id) :review-status))
        (should-error (godel-reviewer-walkthrough (godel-task-get id)) :type 'user-error)
        (should-not (gethash id godel-reviewer--briefing-runs))))))

(ert-deftest godel-test-review-error-after-briefing-ends-the-review ()
  "Whatever fails once the author has answered marks the review failed, not running."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _)
                   (funcall callback 0 (godel-test-briefing-output godel-test-briefing))))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (&rest _) (error "cannot start claude"))))
        (godel-reviewer-run (godel-task-get id)))
      (let ((task (godel-task-get id)))
        (should (eq (plist-get task :review-status) 'failed))
        (should (string-match-p "cannot start claude" (plist-get task :review-note)))))))

(ert-deftest godel-test-send-prompt-survives-a-closed-session ()
  (let (pasted)
    (cl-letf (((symbol-function 'ghostel-paste-string) (lambda (text) (push text pasted)))
              ((symbol-function 'ghostel-send-key) #'ignore))
      (let ((dead (generate-new-buffer "dead-session")))
        (kill-buffer dead)
        (godel-reviewer--send-prompt dead "hello")
        (should-not pasted))
      (let ((live (generate-new-buffer "live-session")))
        (unwind-protect
            (progn (godel-reviewer--send-prompt live "hello")
                   (should (equal pasted '("hello"))))
          (kill-buffer live))))))

(ert-deftest godel-test-reviewer-run-asks-the-author-first ()
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           order activity-while-asking stream-input finish-asking)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _)
                   (push 'asked order)
                   (setq finish-asking
                         (lambda () (funcall callback 0 (godel-test-briefing-output godel-test-briefing))))
                   'proc))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a input _e _m on-exit)
                   (push 'reviewed order)
                   (setq stream-input input)
                   (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id))
        ;; Asking: the board says so, and the review hasn't started.
        (setq activity-while-asking (plist-get (godel-task-get id) :review-activity))
        (should (eq (plist-get (godel-task-get id) :review-status) 'running))
        (should (equal order '(asked)))
        (funcall finish-asking))
      (should (equal activity-while-asking "asking the author"))
      (should (equal (reverse order) '(asked reviewed)))
      (should (string-match-p "<briefing>\n## Problem\nUsers got logged out" stream-input))
      (should (string-match-p "^\\+    return 20" stream-input))
      (let ((task (godel-task-get id)))
        (should (eq (plist-get task :review-status) 'done))
        (should (equal (plist-get task :review-note) "2 findings (briefed by the author)"))
        (should (equal (plist-get task :review-briefing) godel-test-briefing)))
      ;; Reviewing again with a fresh task state forgets the old briefing.
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-reviewer-briefing)
                 (lambda (_task callback) (funcall callback nil nil)))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a input _e _m on-exit)
                   (setq stream-input input)
                   (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id)))
      (should-not (string-match-p "briefing" stream-input))
      (should-not (plist-get (godel-task-get id) :review-briefing))
      (should (equal (plist-get (godel-task-get id) :review-note) "2 findings")))))

(ert-deftest godel-test-reviewer-run-without-briefing-says-so ()
  "If the author can't answer, the review still runs, and its note says why."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           stream-input)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _)
                   (funcall callback 1 (godel-test-briefing-output "overloaded" t))))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a input _e _m on-exit)
                   (setq stream-input input)
                   (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id)))
      (should-not (string-match-p "briefing" stream-input))
      (should (string-match-p "return 20" stream-input))
      (let ((task (godel-task-get id)))
        (should (eq (plist-get task :review-status) 'done))
        (should (equal (plist-get task :review-note)
                       "2 findings (the author couldn't brief it: overloaded)"))
        (should-not (plist-get task :review-briefing))))))

(ert-deftest godel-test-review-setup-failure-ends-the-review ()
  "A failure setting up the briefing, before any process starts, fails the review
too, instead of leaving it marked as running and refusing every later `v r'."
  (godel-test-with-env
    (let* (;; A malformed stored effort on the author: the briefing's own
           ;; launch setup signals before it starts anything.
           (task (godel-test-briefed-task tmp :model "sonnet" :effort 5))
           (id (plist-get task :id))
           stream-input)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-models--run)
                 (lambda (&rest _) (error "Must not start a process")))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a input _e _m on-exit)
                   (setq stream-input input)
                   (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id))
        (let ((task (godel-task-get id)))
          (should (eq (plist-get task :review-status) 'failed))
          (should (plist-get task :review-note)))
        (should-not stream-input)
        ;; Not stuck: once the stored effort is fixed, a review runs.
        (godel-task-update id :model "notdiamond[1m]" :effort "low")
        (cl-letf (((symbol-function 'godel-models--run)
                   (lambda (_p _a callback &rest _)
                     (funcall callback 0 (godel-test-briefing-output godel-test-briefing))
                     'proc)))
          (godel-reviewer-run (godel-task-get id))))
      (should (eq (plist-get (godel-task-get id) :review-status) 'done)))))

(ert-deftest godel-test-review-diff-is-as-fresh-as-the-files ()
  "The author can keep working while it writes the briefing: the reviewer gets
the diff as it is when the briefing arrives, matching the files it will read."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           answer stream-input)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _) (setq answer callback) 'proc))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a input _e _m on-exit)
                   (setq stream-input input)
                   (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id))
        ;; The author goes on working while it's asked.
        (with-temp-buffer
          (insert "\ndef d():\n    return 4\n")
          (append-to-file (point-min) (point-max)
                          (expand-file-name "app.py" (plist-get task :worktree))))
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing)))
      (should (string-match-p "^\\+    return 4" stream-input))
      (should (string-match-p "^\\+    return 20" stream-input)))
    ;; If the diff can't be taken again, the earlier one is used, not a failure.
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           (calls 0) answer stream-input)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-reviewer-diff-text)
                 (lambda (_task) (if (= (cl-incf calls) 1) "EARLIER-DIFF\n" (user-error "gone"))))
                ((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _) (setq answer callback) 'proc))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a input _e _m on-exit)
                   (setq stream-input input)
                   (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id))
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing)))
      (should (string-match-p "EARLIER-DIFF" stream-input))
      (should (eq (plist-get (godel-task-get id) :review-status) 'done)))))

(ert-deftest godel-test-reviewer-run-task-gone-while-asking ()
  "A task torn down while its author answers doesn't start a review."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _)
                   (godel-task-delete id)
                   (funcall callback 0 (godel-test-briefing-output godel-test-briefing))))
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (&rest _) (error "Must not review a gone task"))))
        (godel-reviewer-run (godel-task-get id)))
      (should-not (godel-task-get id)))))

;; The walkthrough opens its session only after the author's briefing is back.

(defvar godel-test--workspace nil "The workspace `godel-test-with-workspaces' says is current.")
(defvar godel-test--switches nil "Workspaces switched to, newest first.")

(defmacro godel-test-with-workspaces (current existing &rest body)
  "Run BODY with Doom's workspace functions faked: CURRENT is the current
workspace and EXISTING the ones that exist."
  (declare (indent 2))
  `(let ((godel-test--workspace ,current) (godel-test--switches nil))
     (cl-letf (((symbol-function '+workspace-current-name) (lambda () godel-test--workspace))
               ((symbol-function '+workspace-exists-p) (lambda (name) (member name ,existing)))
               ((symbol-function '+workspace-switch)
                (lambda (name &optional _create)
                  (push name godel-test--switches)
                  (error "godel must not switch workspaces under you (to %s)" name))))
       ,@body)))

(defmacro godel-test-with-walkthrough-stubs (&rest body)
  "Run BODY with the walkthrough's outside world faked.
Binds `answer' (the author's pending reply), `asks' (how often the author was
asked), `opened' (the session's callback), `opened-focus', `opens' (how many
sessions were opened), `sent' (buffer and prompt), `sends', `notified' and
`messages'."
  (declare (indent 0))
  `(let (answer opened opened-focus (opens 0) (asks 0) sent (sends 0) notified messages)
     (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
               ((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
               ((symbol-function 'godel-notify) (lambda (title body) (push (list title body) notified)))
               ((symbol-function 'godel-models--run)
                (lambda (_p _a callback &rest _) (cl-incf asks) (setq answer callback) 'proc))
               ((symbol-function 'godel-reviewer--start-session)
                (lambda (_task _launch callback &optional focus)
                  (cl-incf opens)
                  (setq opened callback opened-focus (list focus))))
               ((symbol-function 'godel-reviewer--send-prompt)
                (lambda (buffer prompt) (cl-incf sends) (setq sent (list buffer prompt)))))
       ,@body)))

(ert-deftest godel-test-walkthrough-opens-after-briefing ()
  "While the author writes, the board says so and no session is open yet."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp :review-briefing "the review's own briefing"))
           (id (plist-get task :id))
           (store (godel-task--store-file (plist-get task :repo)))
           (keys (lambda () (cl-loop for (k _) on (godel-task-get id) by #'cddr collect k)))
           (file-text (lambda () (with-temp-buffer (insert-file-contents store) (buffer-string))))
           (keys-before (funcall keys))
           (file-before (funcall file-text))
           badge-during)
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        ;; Asking: the automatic reviewer's note, and no session yet.
        (should-not opened)
        (should (godel-reviewer-asking-since (godel-task-get id)))
        ;; The note is in memory only: the task and its file on disk are
        ;; exactly as they were before `v w'.
        (should (equal (funcall keys) keys-before))
        (should (equal (funcall file-text) file-before))
        (should (godel-reviewer--busy-p (godel-task-get id)))
        (setq badge-during (cadr (godel-board--review-badge (godel-task-get id))))
        (should (string-match-p "\\`reviewing [0-9]+s · asking the author\\'" badge-during))
        (should (cl-some (lambda (m) (string-match-p "asking the author for a briefing" m)) messages))
        ;; The briefing arrives: the note goes, the session opens, and only
        ;; then is the first message sent.
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
        (should-not (godel-reviewer-asking-since (godel-task-get id)))
        (should-not (godel-reviewer--busy-p (godel-task-get id)))
        (should opened)
        (should-not sent)
        ;; It had to wait, so it must not take the focus.
        (should (equal opened-focus '(nil)))
        (funcall opened 'the-buffer)
        (should (eq (car sent) 'the-buffer))
        (should (string-match-p "<briefing>\n## Problem" (cadr sent)))
        (should (string-match-p "^\\+    return 20" (cadr sent)))
        (should-not (string-match-p "asking" (or (cadr (godel-board--review-badge (godel-task-get id)))
                                                  "")))
        ;; And a notification says it's ready.
        (should (equal (caar notified) "godel: walkthrough of rev is ready")))
      ;; The review's briefing is the review's: the walkthrough leaves it alone.
      (should (equal (plist-get (godel-task-get id) :review-briefing) "the review's own briefing"))
      ;; And afterwards it left nothing in the store either.
      (should (equal (funcall keys) keys-before)))))

(ert-deftest godel-test-walkthrough-diff-is-as-fresh-as-the-files ()
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        (with-temp-buffer
          (insert "\ndef d():\n    return 4\n")
          (append-to-file (point-min) (point-max)
                          (expand-file-name "app.py" (plist-get task :worktree))))
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
        (funcall opened 'the-buffer)
        (should (string-match-p "^\\+    return 4" (cadr sent)))
        (should (string-match-p "^\\+    return 20" (cadr sent)))))))

(ert-deftest godel-test-walkthrough-session-not-ready ()
  "If the session never shows its input box, nothing is typed into it."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
        (funcall opened :not-ready)
        (should-not sent)
        (should (cl-some (lambda (m) (string-match-p "isn't ready" m)) messages))
        (should-not notified)
        ;; Nothing is left starting: the next `v w' is allowed.
        (should-not (gethash id godel-reviewer--walkthrough-starting))))))

(ert-deftest godel-test-walkthrough-session-never-started ()
  "No session to type into: only the failure to start is reported, not
\"type your request there\"."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
        (funcall opened nil)
        (should-not sent)
        (should-not notified)
        (should-not (cl-some (lambda (m) (string-match-p "type your request" m)) messages))
        (should-not (gethash id godel-reviewer--walkthrough-starting))))))

(ert-deftest godel-test-start-session-callback-tells-the-cases-apart ()
  "The buffer when ready, `:not-ready' when it never showed its input box, nil
(with a message) when it never started or closed first."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (launch (godel-models-launch-env (godel-models-find "notdiamond[1m]") "medium"))
           (name (godel-agent-buffer-name (plist-get task :worktree)
                                          godel-reviewer-walkthrough-instance))
           (screen nil) (create t) (close-first nil) got messages)
      (require 'claude-code-ide)
      (unwind-protect
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
                    ((symbol-function 'claude-code-ide)
                     (lambda () (when create (generate-new-buffer name))))
                    ((symbol-function 'godel-tui-wait)
                     (lambda (buffer _pred _timeout callback)
                       (when close-first (kill-buffer buffer))
                       (funcall callback screen))))
            (let ((run (lambda ()
                         (when-let* ((b (get-buffer name))) (kill-buffer b))
                         (setq got 'unset messages nil)
                         (godel-reviewer--start-session task launch (lambda (b) (setq got b))))))
              ;; Never started.
              (setq create nil)
              (funcall run)
              (should (null got))
              (should (cl-some (lambda (m) (string-match-p "didn't start" m)) messages))
              ;; Ready.
              (setq create t screen "❯ ")
              (funcall run)
              (should (eq got (get-buffer name)))
              ;; Started, but the input box never showed.
              (setq screen nil)
              (funcall run)
              (should (eq got :not-ready))
              ;; Closed before it was ready.
              (setq close-first t)
              (funcall run)
              (should (null got))
              (should (cl-some (lambda (m) (string-match-p "closed before it was ready" m)) messages))))
        (when-let* ((b (get-buffer name))) (kill-buffer b))))))

(ert-deftest godel-test-walkthrough-without-briefing-still-starts ()
  "A failed briefing doesn't keep the walkthrough closed: it opens, without the briefing."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        (funcall answer 1 (godel-test-briefing-output "overloaded" t))
        (should opened)
        (should-not (godel-reviewer-asking-since (godel-task-get id)))
        (funcall opened 'the-buffer)
        (should (string-match-p "return 20" (cadr sent)))
        (should-not (string-match-p "briefing" (cadr sent)))
        (should (cl-some (lambda (m) (string-match-p "couldn't brief the walkthrough (overloaded)" m))
                         messages)))
      (should-not (plist-get (godel-task-get id) :review-briefing)))))

(ert-deftest godel-test-walkthrough-fresh-task-opens-at-once ()
  "A task whose session never started has no author to ask: nothing waits,
and since you've only just asked, the session may take the focus."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        (should-not answer)
        (should opened)
        (should (equal opened-focus '(t)))
        (should-not (godel-reviewer-asking-since (godel-task-get id)))
        (funcall opened 'the-buffer)
        (should (eq (car sent) 'the-buffer))
        (should-not (string-match-p "briefing" (cadr sent)))
        (should-not notified)))))

(ert-deftest godel-test-walkthrough-task-gone-while-asking ()
  "A task torn down while its author answers: no session, no stale note."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        (should (godel-reviewer-asking-since (godel-task-get id)))
        (godel-task-delete id)
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
        (should-not opened))
      (should-not (gethash id godel-reviewer--asking))
      (should-not (gethash id godel-reviewer--walkthrough-starting)))))

(ert-deftest godel-test-walkthrough-second-press-is-refused-while-starting ()
  "Two `v w' in a row mustn't open two sessions or paste the first message twice."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        ;; Refused while the author is still writing...
        (should-error (godel-reviewer-walkthrough (godel-task-get id)) :type 'user-error)
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
        ;; ...and while the session is still coming up.
        (should-error (godel-reviewer-walkthrough (godel-task-get id)) :type 'user-error)
        (funcall opened 'the-buffer)
        (should (= opens 1))
        (should (= sends 1))
        ;; Done starting: a new walkthrough is allowed (nothing is open).
        (should-not (gethash id godel-reviewer--walkthrough-starting))
        (godel-reviewer-walkthrough (godel-task-get id))
        (should (godel-reviewer-asking-since (godel-task-get id)))))))

(ert-deftest godel-test-walkthrough-second-press-is-refused-before-the-proxy-answers ()
  "The window between the key press and the proxy check is covered too."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           proxy-callback)
      (godel-test-with-walkthrough-stubs
        (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (setq proxy-callback cb))))
          (godel-reviewer-walkthrough (godel-task-get id))
          (should-error (godel-reviewer-walkthrough (godel-task-get id)) :type 'user-error)
          (funcall proxy-callback t))
        (should (godel-reviewer-asking-since (godel-task-get id)))))))

(ert-deftest godel-test-walkthrough-already-open-is-shown-not-restarted ()
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           (name (godel-agent-buffer-name (plist-get task :worktree) godel-reviewer-walkthrough-instance))
           (buffer (generate-new-buffer name))
           shown)
      (unwind-protect
          (progn
            (make-process :name "godel-test-session" :buffer buffer :command '("sleep" "30") :noquery t)
            (godel-test-with-walkthrough-stubs
              (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq shown b)))
                        ((symbol-function 'godel-models-ensure-proxy)
                         (lambda (&rest _) (error "Must not start another"))))
                (godel-reviewer-walkthrough (godel-task-get id)))
              (should (eq shown buffer))
              (should (= opens 0))
              (should-not answer)
              (should (cl-some (lambda (m) (string-match-p "already open" m)) messages)))
            (should-not (gethash id godel-reviewer--walkthrough-starting)))
        (when-let* ((proc (get-buffer-process buffer))) (delete-process proc))
        (kill-buffer buffer)))))

(ert-deftest godel-test-walkthrough-failures-free-the-next-press ()
  "A failure anywhere in starting must not leave `v w' refused for good."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      ;; The proxy won't start.
      (godel-test-with-walkthrough-stubs
        (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb nil "no proxy"))))
          (godel-reviewer-walkthrough (godel-task-get id)))
        (should (cl-some (lambda (m) (string-match-p "not started: no proxy" m)) messages))
        (should-not (gethash id godel-reviewer--walkthrough-starting)))
      ;; Opening the session fails.
      (godel-test-with-walkthrough-stubs
        (cl-letf (((symbol-function 'godel-reviewer--start-session)
                   (lambda (&rest _) (error "claude-code-ide is broken"))))
          (godel-reviewer-walkthrough (godel-task-get id))
          (funcall answer 0 (godel-test-briefing-output godel-test-briefing)))
        (should (cl-some (lambda (m) (string-match-p "couldn't start: claude-code-ide is broken" m))
                         messages))
        (should-not (gethash id godel-reviewer--walkthrough-starting))
        (should-not (godel-reviewer-asking-since (godel-task-get id))))
      ;; Something unexpected right after the proxy check.
      (godel-test-with-walkthrough-stubs
        (cl-letf (((symbol-function 'godel-reviewer-briefing)
                   (lambda (&rest _) (error "surprise"))))
          (godel-reviewer-walkthrough (godel-task-get id)))
        (should (cl-some (lambda (m) (string-match-p "couldn't start: surprise" m)) messages))
        (should-not (gethash id godel-reviewer--walkthrough-starting))
        (should-not (godel-reviewer-asking-since (godel-task-get id))))
      ;; After all that, it still starts.
      (godel-test-with-walkthrough-stubs
        (godel-reviewer-walkthrough (godel-task-get id))
        (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
        (funcall opened 'the-buffer)
        (should (= sends 1))))))

(ert-deftest godel-test-walkthrough-moved-on-keeps-the-briefing ()
  "You left the workspace while the author wrote: nothing opens under you, the
briefing is kept, and the next `v w' where you were opens at once with it."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-workspaces "task-ws" '("task-ws" "other-ws")
        (godel-test-with-walkthrough-stubs
          (godel-reviewer-walkthrough (godel-task-get id))
          (setq godel-test--workspace "other-ws")             ; the user moves on
          (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
          ;; Nothing opened, no switching, the guard is free, the briefing kept.
          (should-not opened)
          (should-not godel-test--switches)
          (should (equal godel-test--workspace "other-ws"))
          (should-not (gethash id godel-reviewer--walkthrough-starting))
          (should (gethash id godel-reviewer--prepared))
          (should (equal (caar notified) "godel: walkthrough of rev is ready to start"))
          (should (string-match-p "Press v w in task-ws" (cadr (car notified))))
          ;; Back in the workspace: `v w' opens at once, with the kept
          ;; briefing, without asking again, and with the focus (you just asked).
          (setq godel-test--workspace "task-ws" notified nil)
          (godel-reviewer-walkthrough (godel-task-get id))
          (should (= asks 1))
          (should opened)
          (should (equal opened-focus '(t)))
          (funcall opened 'the-buffer)
          (should (string-match-p "<briefing>\n## Problem" (cadr sent)))
          (should-not notified)
          (should-not godel-test--switches)
          ;; Used up: a third walkthrough asks again.
          (should-not (gethash id godel-reviewer--prepared))
          (godel-reviewer-walkthrough (godel-task-get id))
          (should (= asks 2)))))))

(ert-deftest godel-test-walkthrough-kept-briefing-expires ()
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-workspaces "task-ws" '("task-ws" "other-ws")
        (godel-test-with-walkthrough-stubs
          (godel-reviewer-walkthrough (godel-task-get id))
          (setq godel-test--workspace "other-ws")
          (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
          (setq godel-test--workspace "task-ws")
          ;; Too old to trust: ask again.
          (let ((godel-reviewer-prepared-max-age 0))
            (godel-reviewer-walkthrough (godel-task-get id)))
          (should (= asks 2))
          (should-not opened))))))

(ert-deftest godel-test-walkthrough-in-the-minibuffer-doesnt-open-under-you ()
  "Mid-prompt when the briefing arrives: the same, nothing opens."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           (in-minibuffer nil))
      (godel-test-with-walkthrough-stubs
        (cl-letf (((symbol-function 'active-minibuffer-window)
                   (lambda () (and in-minibuffer (selected-window)))))
          (godel-reviewer-walkthrough (godel-task-get id))
          (setq in-minibuffer t)                                ; M-x, a prompt...
          (funcall answer 0 (godel-test-briefing-output godel-test-briefing))
          (should-not opened)
          (should (gethash id godel-reviewer--prepared))
          (should (string-match-p "ready to start" (caar notified)))
          (setq in-minibuffer nil)
          (godel-reviewer-walkthrough (godel-task-get id))
          (should opened)
          (should (= asks 1)))))))

(ert-deftest godel-test-walkthrough-moved-on-and-no-briefing ()
  "Moved on and the author couldn't brief it: the fact is kept, so the next
`v w' opens at once, without a briefing and without asking the author again."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id)))
      (godel-test-with-workspaces "task-ws" '("task-ws" "other-ws")
        (godel-test-with-walkthrough-stubs
          (godel-reviewer-walkthrough (godel-task-get id))
          (setq godel-test--workspace "other-ws")
          (funcall answer 1 (godel-test-briefing-output "overloaded" t))
          (should-not opened)
          ;; The notification says what the next press will do.
          (should (string-match-p "couldn't brief it, so it opens without one"
                                  (cadr (car notified))))
          ;; Kept, with no briefing in it.
          (should (consp (gethash id godel-reviewer--prepared)))
          (should-not (cdr (gethash id godel-reviewer--prepared)))
          (setq godel-test--workspace "task-ws" notified nil)
          (godel-reviewer-walkthrough (godel-task-get id))
          (should (= asks 1))                     ; not asked again
          (should opened)
          (should (equal opened-focus '(t)))
          (funcall opened 'the-buffer)
          (should (string-match-p "return 20" (cadr sent)))
          (should-not (string-match-p "briefing" (cadr sent)))
          (should-not (gethash id godel-reviewer--prepared)))))))

(ert-deftest godel-test-briefing-request-speaks-to-the-author-about-a-third-reader ()
  "The request is read by an author writing for a reviewer: the user is \"the
user\", the author is \"I\", and nothing in it is addressed to the author as \"you\"."
  ;; Whitespace folded: the docstring is wrapped across lines.
  (let ((request (replace-regexp-in-string "[ \n\t]+" " " godel-reviewer--briefing-request)))
    (should (string-match-p "not from the user" request))
    (should (string-match-p "\"the user\"" request))
    (should (string-match-p "\"I\"" request))
    (should (string-match-p "\"the reviewer\"" request))
    (should (string-match-p "Don't change any files" request))
    ;; The old wording, addressed to the author as the user.
    (should-not (string-match-p "you and I\\|what I told you\\|requirements I added\\|yours and mine" request))
    (should (string-match-p "## Problem" request))
    (should (string-match-p "## Solution" request))
    (should (string-match-p "## Tested" request))))

(ert-deftest godel-test-walkthrough-hung-start-doesnt-refuse-for-good ()
  "A proxy check that never answers: after the deadline `v w' works again, and the
abandoned attempt, if it ever wakes up, does nothing."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           old-proxy-callback)
      (godel-test-with-walkthrough-stubs
        (cl-letf (((symbol-function 'godel-models-ensure-proxy)
                   (lambda (cb) (unless old-proxy-callback (setq old-proxy-callback cb)))))
          (godel-reviewer-walkthrough (godel-task-get id))
          (should-error (godel-reviewer-walkthrough (godel-task-get id)) :type 'user-error)
          ;; Time passes; nothing ever answers.
          (setf (car (gethash id godel-reviewer--walkthrough-starting))
                (- (float-time) (1+ (godel-reviewer--walkthrough-deadline))))
          (should-not (godel-reviewer--walkthrough-starting-p id))
          (godel-reviewer-walkthrough (godel-task-get id)))          ; allowed again
        (should (gethash id godel-reviewer--walkthrough-starting))
        (let ((new-token (gethash id godel-reviewer--walkthrough-starting)))
          ;; The first attempt's proxy check finally answers: ignored.
          (funcall old-proxy-callback t)
          (should (= asks 0))
          (should-not opened)
          (should (eq (gethash id godel-reviewer--walkthrough-starting) new-token)))))))

(ert-deftest godel-test-walkthrough-abandoned-attempt-cant-free-the-new-one ()
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           (old (godel-reviewer--walkthrough-begin id))
           (new (godel-reviewer--walkthrough-begin id)))
      (godel-reviewer--walkthrough-done id old)
      (should (eq (gethash id godel-reviewer--walkthrough-starting) new))
      (godel-reviewer--walkthrough-done id new)
      (should-not (gethash id godel-reviewer--walkthrough-starting)))))

(ert-deftest godel-test-walkthrough-session-focus ()
  "The session takes the focus only when asked to."
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (launch (godel-models-launch-env (godel-models-find "notdiamond[1m]") "medium"))
           seen)
      (require 'claude-code-ide)
      (cl-letf (((symbol-function 'claude-code-ide)
                 (lambda () (push claude-code-ide-focus-on-open seen)))
                ((symbol-function 'message) #'ignore))
        (godel-reviewer--start-session task launch #'ignore)
        (godel-reviewer--start-session task launch #'ignore t))
      (should (equal (reverse seen) '(nil t))))))

;;;; Review points: effort fallback, one-line reasons, restart

(ert-deftest godel-test-effort-pickers-offer-all-levels-without-a-list ()
  "A model entry that doesn't say which efforts it supports still gets a choice."
  (let ((bare '(:value "notdiamond[1m]" :name "Not Diamond" :resolved "notdiamond[1m]")))
    (should (equal (godel-models-efforts bare) godel-effort-levels))
    (should (equal (godel-models-efforts '(:efforts ("low" "high"))) '("low" "high"))))
  (godel-test-with-env
    (let* ((task (godel-test-started-task tmp 'idle))
           (id (plist-get task :id))
           (bare '(:value "notdiamond[1m]" :name "Not Diamond" :resolved "notdiamond[1m]"))
           offered)
      (cl-letf (((symbol-function 'godel-models-find) (lambda (&rest _) bare))
                ((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _) (setq offered coll) "low")))
        ;; The new-task menu.
        (godel-new--read-effort "Effort: " nil nil)
        (should (equal offered godel-effort-levels))
        ;; R on the board.
        (setq offered nil)
        (cl-letf (((symbol-function 'godel-board--task) (lambda () (godel-task-get id)))
                  ((symbol-function 'godel-new--read-model) (lambda (&rest _) "notdiamond[1m]"))
                  ((symbol-function 'godel-agent-live-p) #'ignore)
                  ((symbol-function 'godel-agent-stop) #'ignore)
                  ((symbol-function 'run-with-timer) #'ignore))
          (godel-board-restart))
        (should (equal offered godel-effort-levels))
        (should (equal (plist-get (godel-task-get id) :effort) "low"))))))

(ert-deftest godel-test-briefing-reason-is-one-short-line ()
  "An API error can be long and many lines; the note gets one line, without a repeated prefix."
  (should (equal (cadr (should-error (godel-reviewer-parse-briefing
                                      (godel-test-briefing-output "overloaded" t))))
                 "overloaded"))
  (should (equal (cadr (should-error (godel-reviewer-parse-briefing "no json\n"))) "no answer"))
  (should (equal (cadr (should-error (godel-reviewer-parse-briefing (godel-test-briefing-output "  "))))
                 "the reply was empty"))
  (should (equal (godel-reviewer--short-reason "API Error: 529\nrequest id: abc\n{\"big\": \"json\"}")
                 "API Error: 529"))
  (should (equal (godel-reviewer--short-reason "") "no details"))
  (let ((short (godel-reviewer--short-reason (make-string 500 ?x))))
    (should (<= (string-width short) 80))
    (should (string-suffix-p "…" short)))
  ;; Through the whole path: the waiting reviewer hears the short line.
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp))
           (id (plist-get task :id))
           result)
      (cl-letf (((symbol-function 'godel-models--run)
                 (lambda (_p _a callback &rest _)
                   (funcall callback 1 (godel-test-briefing-output
                                        (concat "API Error: 529 overloaded\nrequest id: abc\n"
                                                (make-string 300 ?y))
                                        t)))))
        (godel-reviewer-briefing (godel-task-get id) (lambda (&rest r) (setq result r))))
      (should (equal result '(nil "API Error: 529 overloaded"))))))

(ert-deftest godel-test-restart-fails-a-review-that-was-running ()
  "A review running when Emacs went away can't finish: it's failed after a restart,
so `v r' isn't refused for good."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (repo (plist-get task :repo))
           (done-task (godel-task-create :title "done" :repo repo :base "main")))
      (godel-task-update id :review-status 'running :review-activity "asking the author"
                         :review-started (float-time))
      (godel-task-update (plist-get done-task :id) :review-status 'done :review-note "2 findings")
      ;; A restart: nothing in memory, everything read back from the store.
      (clrhash godel-task--tasks)
      (setq godel-task--loaded-repos nil)
      (godel-task-load-repo repo)
      (let ((task (godel-task-get id)))
        (should (eq (plist-get task :review-status) 'failed))
        (should (equal (plist-get task :review-note) "interrupted by a restart"))
        (should-not (plist-get task :review-activity)))
      ;; Finished reviews are as they were.
      (should (eq (plist-get (godel-task-get (plist-get done-task :id)) :review-status) 'done))
      (should (equal (plist-get (godel-task-get (plist-get done-task :id)) :review-note) "2 findings"))
      ;; And a review can be started again.
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a _i _e _m on-exit) (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id)))
      (should (eq (plist-get (godel-task-get id) :review-status) 'done)))))

(ert-deftest godel-test-review-list-shows-briefing ()
  (godel-test-with-env
    (let* ((task (godel-test-briefed-task tmp :review-briefing godel-test-briefing))
           (id (plist-get task :id)))
      (save-window-excursion
        (godel-review-list (godel-task-get id))
        (unwind-protect
            (progn
              (goto-char (point-min))
              (search-forward "The author's briefing to the reviewer")
              (let* ((section (magit-current-section))
                     ;; Where the briefing's text is.  `search-forward' finds
                     ;; hidden text too, so ask whether it's actually shown.
                     (text-pos (lambda (text)
                                 (save-excursion (goto-char (point-min))
                                                 (search-forward text)
                                                 (match-beginning 0)))))
                ;; Hidden until TAB...
                (should (oref section hidden))
                (should (invisible-p (funcall text-pos "Users got logged out")))
                (should (invisible-p (funcall text-pos "the user chose 5 min")))
                ;; ...then shown.
                (magit-section-show section)
                (should-not (oref section hidden))
                (should-not (invisible-p (funcall text-pos "Users got logged out")))
                (should-not (invisible-p (funcall text-pos "the user chose 5 min")))
                ;; And hidden again.
                (magit-section-hide section)
                (should (invisible-p (funcall text-pos "Users got logged out")))))
          (kill-buffer (godel-review--list-buffer-name id)))))))

(ert-deftest godel-test-review-list-findings ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id)))
      (add-hook 'godel-event-functions #'godel-review--refresh-lists)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-notify) #'ignore)
                ((symbol-function 'godel-reviewer--stream)
                 (lambda (_p _a _i _e _m on-exit) (funcall on-exit 0 godel-test-reviewer-output))))
        (godel-reviewer-run (godel-task-get id)))
      (save-window-excursion
        (godel-review-list (godel-task-get id))
        (unwind-protect
            (progn
              (should (godel-test-text-order-p "Independent review: 2 findings"
                                               "Reviewer findings (2)" "high" "app.py:5"
                                               "low" "new.txt:1"))
              (goto-char (point-min))
              (search-forward "app.py:5")
              (godel-review-list-accept)
              (should (godel-test-text-order-p "Reviewer findings (1)" "Pending (1)"
                                               "[reviewer, high]")))
          (kill-buffer (godel-review--list-buffer-name id)))))))

(ert-deftest godel-test-ship-draft-parse-and-split ()
  (let ((out (concat "wrapper line\n"
                     (json-encode '((type . "result") (is_error . :json-false)
                                    (result . "<commit>Add greet\n\nReturns a greeting.</commit>\n<pr-title>Add greet()</pr-title>\n<pr-body>Adds `greet`.</pr-body>")))
                     "\n")))
    (should (equal (godel-ship-parse-draft out)
                   '("Add greet\n\nReturns a greeting." "Add greet()" "Adds `greet`.")))
    (should-error (godel-ship-parse-draft "{\"type\":\"result\",\"is_error\":true,\"result\":\"no\"}")))
  (let ((text (godel-ship-draft-text '("Subject\n\nBody" "PR title" "PR body\nmore") nil)))
    (should (equal (godel-ship-split text) '("Subject\n\nBody" "PR title" "PR body\nmore"))))
  (let ((text (godel-ship-draft-text '("Fix typo" nil nil) "https://x/pr/1")))
    (should-not (string-match-p "Pull request" text))
    (should (equal (godel-ship-split text) '("Fix typo" nil nil)))))

(ert-deftest godel-test-ship-draft-forks-author-session ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           ran)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-models--run)
                 (lambda (_program args callback &optional input env)
                   (setq ran (list :args args :input input :env env))
                   (funcall callback 0 "{\"type\":\"result\",\"is_error\":false,\"result\":\"<commit>c</commit>\"}"))))
        (godel-task-update id :session-id "sid-s" :transcript "/tmp/t.jsonl")
        (godel-ship-draft (godel-task-get id) #'ignore)
        (should (equal (cadr (member "--resume" (plist-get ran :args))) "sid-s"))
        (should (member "--fork-session" (plist-get ran :args)))
        (should (member "--no-session-persistence" (plist-get ran :args)))
        (should (equal (car (plist-get ran :env)) "GODEL_SERVER"))
        (godel-task-update id :transcript nil)
        (godel-ship-draft (godel-task-get id) #'ignore)
        (should-not (member "--resume" (plist-get ran :args)))
        (should (string-match-p "return 20" (plist-get ran :input)))))))

(ert-deftest godel-test-ship-shows-drafting ()
  "The compose buffer opens at once, waiting (read-only, can't submit),
the board badge shows the draft; the draft then fills the buffer in."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           finish buffer)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-models--run)
                 (lambda (_program _args callback &rest _)
                   (setq finish callback)
                   (start-process "godel-test-sleep" nil "sleep" "30")))
                ((symbol-function 'godel-notify) #'ignore))
        (unwind-protect
            (progn
              (setq buffer (godel-ship (godel-task-get id)))
              (with-current-buffer buffer
                (should buffer-read-only)
                (should (string-match-p "drafting the commit message and PR"
                                        (overlay-get godel-compose--waiting 'before-string)))
                (should-error (godel-compose-submit) :type 'user-error))
              (should (godel-ship-drafting (godel-task-get id)))
              (should (string-prefix-p "drafting the ship message"
                                       (cadr (godel-board--review-badge (godel-task-get id)))))
              (funcall finish 0 "{\"type\":\"result\",\"is_error\":false,\"result\":\"<commit>Subject</commit><pr-title>T</pr-title><pr-body>B</pr-body>\"}")
              (should-not (godel-ship-drafting (godel-task-get id)))
              (should-not (godel-board--review-badge (godel-task-get id)))
              (with-current-buffer buffer
                (should-not buffer-read-only)
                (should-not godel-compose--waiting)
                (should (string-prefix-p "Subject\n" (buffer-string)))
                (should (string-match-p "drafted by the author" (format "%s" header-line-format)))))
          (when (buffer-live-p buffer)
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))
          (godel-ship-cancel-draft id))))))

(ert-deftest godel-test-ship-cancel-stops-draft ()
  "Cancelling while the draft runs stops the Claude process; its result is
dropped."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           proc finish called)
      (cl-letf (((symbol-function 'godel-models-ensure-proxy) (lambda (cb) (funcall cb t)))
                ((symbol-function 'godel-models--run)
                 (lambda (_program _args callback &rest _)
                   (setq finish callback
                         proc (start-process "godel-test-sleep" nil "sleep" "30")))))
        (godel-ship-draft (godel-task-get id) (lambda (&rest _) (setq called t)))
        (should (process-live-p proc))
        (godel-ship-cancel-draft id)
        (should-not (process-live-p proc))
        (should-not (godel-ship-drafting (godel-task-get id)))
        (funcall finish 0 "{}")
        (should-not called)))))

(defun godel-test-fake-gh (dir url state &optional draft)
  "A fake gh in DIR: pr create prints URL and logs its args; pr view prints
STATE (and isDraft, DRAFT)."
  (let ((gh (expand-file-name "gh" dir)) (log (expand-file-name "gh.log" dir)))
    (with-temp-file gh
      (insert "#!/bin/sh\n"
              "printf '%s\\n' \"$*\" >> " (shell-quote-argument log) "\n"
              "case \"$1 $2\" in\n"
              "  'pr create') echo " (shell-quote-argument url) " ;;\n"
              "  'pr view') echo '{\"state\":\"" state "\",\"isDraft\":"
              (if draft "true" "false") "}' ;;\n"
              "esac\n"))
    (set-file-modes gh #o755)
    (cons gh log)))

(ert-deftest godel-test-ship-execute ()
  "Commit, push to a real (local) remote, and open the PR with gh; tell
the author's (running) session what was shipped."
  (let (sent)
  (cl-letf (((symbol-function 'godel-agent-live-p) (lambda (_) t))
            ((symbol-function 'godel-tui-wait) (lambda (_b _p _t cb) (funcall cb "ready")))
            ((symbol-function 'godel-agent-send) (lambda (_ text) (push text sent))))
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (remote (expand-file-name "remote.git" tmp))
           (fake (godel-test-fake-gh tmp "https://github.com/me/repo/pull/7" "OPEN"))
           (godel-gh-program (car fake))
           (done (lambda () (string-match-p "\\`\\(pushed\\|PR opened\\|.*failed\\)"
                                            (or (plist-get (godel-task-get id) :last-event) "")))))
      (call-process "git" nil nil nil "init" "-q" "--bare" remote)
      (let ((default-directory (plist-get task :repo)))
        (call-process "git" nil nil nil "remote" "add" "origin" remote)
        (call-process "git" nil nil nil "push" "-q" "origin" "main"))
      (let ((default-directory (plist-get task :worktree)))
        (call-process "git" nil nil nil "config" "user.email" "t@t")
        (call-process "git" nil nil nil "config" "user.name" "t"))
      (godel-task-update id :state 'needs-review)
      (godel-ship-execute (godel-task-get id) "Change b and add c\n\nDetails." "Change b" "PR body")
      (should (godel-test-wait done 20))
      (should (equal (plist-get (godel-task-get id) :pr-url) "https://github.com/me/repo/pull/7"))
      (should (equal (plist-get (godel-task-get id) :last-event)
                     "PR opened: https://github.com/me/repo/pull/7"))
      (should (string-match-p (format "committed your changes as %s (\"Change b and add c\") and pushed .*opened a pull request: https://github.com/me/repo/pull/7\\."
                                      (godel-task--git (plist-get task :worktree) "rev-parse" "--short" "HEAD"))
                              (car sent)))
      (should (equal (godel-task--git (plist-get task :worktree) "log" "-1" "--format=%s") "Change b and add c"))
      (should (equal (godel-task--git (plist-get task :worktree) "status" "--porcelain") ""))
      (should (equal (godel-task--git remote "rev-parse" (plist-get task :branch))
                     (godel-task--git (plist-get task :worktree) "rev-parse" "HEAD")))
      (with-temp-buffer
        (insert-file-contents (cdr fake))
        (should (string-match-p (format "pr create --base main --head %s --title Change b --body-file "
                                        (regexp-quote (plist-get task :branch)))
                                (buffer-string))))
      (delete-file (cdr fake))
      (with-temp-file (expand-file-name "more.txt" (plist-get task :worktree)) (insert "more\n"))
      (godel-task-update id :last-event "")
      (godel-ship-execute (godel-task-get id) "More" nil nil)
      (should (godel-test-wait done 20))
      (should (equal (plist-get (godel-task-get id) :last-event) "pushed"))
      (should (string-match-p "which updates the pull request https://github.com/me/repo/pull/7\\." (car sent)))
      (should (= (length sent) 2))
      (should-not (file-exists-p (cdr fake)))
      (should (equal (godel-task--git remote "rev-parse" (plist-get task :branch))
                     (godel-task--git (plist-get task :worktree) "rev-parse" "HEAD")))
      (with-temp-file (expand-file-name "x.txt" (plist-get task :worktree)) (insert "x\n"))
      (should-error (godel-ship-execute (godel-task-get id) nil nil nil) :type 'user-error))))))

(ert-deftest godel-test-ship-note-hooks-registered ()
  (should (memq #'godel-ship--on-session-start godel-status-session-start-functions))
  (should (memq #'godel-ship--on-stop godel-status-stop-functions)))

(ert-deftest godel-test-ship-note-waits-for-session ()
  "A note for an author whose session isn't running (or is busy) is kept
and sent when the session starts or finishes a turn, once."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (live nil) sent)
      (cl-letf (((symbol-function 'godel-agent-live-p) (lambda (_) live))
                ((symbol-function 'godel-tui-wait) (lambda (_b _p _t cb) (funcall cb "ready")))
                ((symbol-function 'godel-agent-send) (lambda (_ text) (push text sent)))
                ((symbol-function 'run-with-timer)
                 (lambda (_secs _repeat fn &rest args) (apply fn args))))
        (godel-task-update id :state 'needs-review :pr-url "https://github.com/me/repo/pull/3")
        ;; Not running: kept.
        (godel-ship--tell-author (godel-task-get id) "Subject" t)
        (should-not sent)
        (should (string-match-p "pull/3" (plist-get (godel-task-get id) :ship-note)))
        ;; The session starts (resumed): delivered, and only once.
        (setq live t)
        (godel-ship--on-session-start (godel-task-get id) t)
        (should (= (length sent) 1))
        (should-not (plist-get (godel-task-get id) :ship-note))
        (run-hook-with-args 'godel-status-stop-functions id)
        (should (= (length sent) 1))
        ;; Busy: kept until the turn ends.
        (godel-task-update id :state 'working)
        (godel-ship--tell-author (godel-task-get id) "Again" nil)
        (should (= (length sent) 1))
        (run-hook-with-args 'godel-status-stop-functions id)
        (should (= (length sent) 2))
        (should (string-match-p "\"Again\"" (car sent)))
        ;; Waiting on a permission prompt: not interrupted.
        (godel-task-update id :state 'needs-attention)
        (godel-ship--tell-author (godel-task-get id) "Third" nil)
        (should (= (length sent) 2))
        (should (plist-get (godel-task-get id) :ship-note))))))

(ert-deftest godel-test-ship-pr-display ()
  "The PR shows on the board row (#N) and in its details; gp opens it."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           opened)
      (godel-task-update id :pr-url "https://github.com/me/repo/pull/308")
      (should (equal (godel-ship-pr-number (godel-task-get id)) "308"))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (godel-ship-browse-pr (godel-task-get id)))
      (should (equal opened "https://github.com/me/repo/pull/308"))
      (with-temp-buffer
        (godel-board-mode)
        (let ((inhibit-read-only t)) (godel-board--insert-task (godel-task-get id)))
        (should (string-match-p "#308" (buffer-string))))
      (with-temp-buffer
        (godel-board-mode)
        (setq godel-board--compact t)
        (let ((inhibit-read-only t)) (godel-board--insert-task-compact (godel-task-get id) nil))
        (let ((row (car (split-string (buffer-string) "\n"))))
          (should (string-match-p "⇡" row))
          (should (<= (string-width row) godel-sidebar-width))))
      (with-temp-buffer
        (godel-board--details (godel-task-get id) 8)
        (should (string-match-p "PR +https://github.com/me/repo/pull/308   gp opens it" (buffer-string))))
      (godel-task-update id :pr-url nil)
      (should-error (godel-ship-browse-pr (godel-task-get id)) :type 'user-error))))

(ert-deftest godel-test-ship-check-merged ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (godel-gh-program (car (godel-test-fake-gh tmp "unused" "MERGED"))))
      (godel-task-update id :pr-url "https://github.com/me/repo/pull/7" :state 'needs-review)
      (godel-ship-check-merged)
      (should (godel-test-wait (lambda () (eq (plist-get (godel-task-get id) :state) 'merged)) 5))
      (should (equal (plist-get (godel-task-get id) :last-event) "PR merged"))
      (with-current-buffer (godel-board-buffer)
        (unwind-protect (should (godel-test-text-order-p "Merged (1)" "rev"))
          (kill-buffer))))))

(ert-deftest godel-test-ship-check-closed ()
  "A PR closed without merging is noted and drawn red; reopened, it's
open again.  gr asks GitHub; a redraw on its own doesn't."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (fake (godel-test-fake-gh tmp "unused" "CLOSED"))
           (godel-gh-program (car fake)))
      (godel-task-update id :pr-url "https://github.com/me/repo/pull/7" :state 'needs-review)
      (godel-board-refresh)             ; not interactive: no gh
      (should-not (file-exists-p (cdr fake)))
      (call-interactively #'godel-board-refresh)
      (should (godel-test-wait (lambda () (eq (plist-get (godel-task-get id) :pr-state) 'closed)) 5))
      (should (eq (plist-get (godel-task-get id) :state) 'needs-review))
      (should (equal (plist-get (godel-task-get id) :last-event) "PR closed without merging"))
      (should (eq (godel-test-face-name (godel-board--pr-face (godel-task-get id))) 'godel-state-needs-attention))
      (should-not (godel-ship-open-pr (godel-task-get id)))
      (with-temp-buffer
        (godel-board--details (godel-task-get id) 8)
        (should (string-match-p "pull/7  (closed without merging)" (buffer-string))))
      ;; Reopened on GitHub.
      (setq godel-gh-program (car (godel-test-fake-gh tmp "unused" "OPEN")))
      (godel-ship-check-merged)
      (should (godel-test-wait (lambda () (eq (plist-get (godel-task-get id) :pr-state) 'open)) 5))
      (should (equal (plist-get (godel-task-get id) :last-event) "PR reopened"))
      (should (eq (godel-test-face-name (godel-board--pr-face (godel-task-get id))) 'godel-state-needs-review)))))

(ert-deftest godel-test-ship-after-closed-pr-opens-new ()
  "With the task's PR closed (or merged), shipping opens a new PR, which
becomes the task's PR; a merged task goes back to needs-review."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (remote (expand-file-name "remote.git" tmp))
           (fake (godel-test-fake-gh tmp "https://github.com/me/repo/pull/9" "OPEN"))
           (godel-gh-program (car fake))
           (done (lambda () (string-match-p "\\`\\(pushed\\|PR opened\\|.*failed\\)"
                                            (or (plist-get (godel-task-get id) :last-event) "")))))
      (call-process "git" nil nil nil "init" "-q" "--bare" remote)
      (let ((default-directory (plist-get task :repo)))
        (call-process "git" nil nil nil "remote" "add" "origin" remote))
      (let ((default-directory (plist-get task :worktree)))
        (call-process "git" nil nil nil "config" "user.email" "t@t")
        (call-process "git" nil nil nil "config" "user.name" "t"))
      (godel-task-update id :pr-url "https://github.com/me/repo/pull/7" :pr-state 'merged
                         :state 'merged :last-event "")
      (godel-ship-execute (godel-task-get id) "Follow-up" "Follow-up PR" "Body")
      (should (godel-test-wait done 20))
      (let ((task (godel-task-get id)))
        (should (equal (plist-get task :pr-url) "https://github.com/me/repo/pull/9"))
        (should (eq (plist-get task :pr-state) 'open))
        (should (eq (plist-get task :state) 'needs-review)))
      (with-temp-buffer
        (insert-file-contents (cdr fake))
        (should (string-match-p "pr create" (buffer-string)))))))

(defun godel-test-face-name (face)
  "FACE's name: the face itself, or the first of a face list."
  (if (consp face) (car face) face))

(defun godel-test-ship-setup (task tmp)
  "A local bare remote for TASK's repo and a committer in its worktree."
  (let ((remote (expand-file-name "remote.git" tmp)))
    (call-process "git" nil nil nil "init" "-q" "--bare" remote)
    (let ((default-directory (plist-get task :repo)))
      (call-process "git" nil nil nil "remote" "add" "origin" remote))
    (let ((default-directory (plist-get task :worktree)))
      (call-process "git" nil nil nil "config" "user.email" "t@t")
      (call-process "git" nil nil nil "config" "user.name" "t"))))

(ert-deftest godel-test-ship-draft-pr ()
  "P d opens a draft PR; shipping it as ready later marks it ready, and
the other way round converts it to a draft."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (fake (godel-test-fake-gh tmp "https://github.com/me/repo/pull/5" "OPEN" t))
           (godel-gh-program (car fake))
           (log (lambda () (with-temp-buffer (insert-file-contents (cdr fake)) (buffer-string))))
           (done (lambda () (string-match-p "\\`\\(pushed\\|Draft PR opened\\|PR opened\\|.*failed\\)"
                                            (or (plist-get (godel-task-get id) :last-event) "")))))
      (godel-test-ship-setup task tmp)
      (godel-ship-execute (godel-task-get id) "Change" "Title" "Body" t)
      (should (godel-test-wait done 20))
      (should (equal (plist-get (godel-task-get id) :last-event)
                     "Draft PR opened: https://github.com/me/repo/pull/5"))
      (should (plist-get (godel-task-get id) :pr-draft))
      (should (string-match-p "pr create .* --draft" (funcall log)))
      (should (eq (godel-test-face-name (godel-board--pr-face (godel-task-get id))) 'godel-state-stopped))
      (with-temp-buffer
        (godel-board--details (godel-task-get id) 8)
        (should (string-match-p "pull/5  (draft)" (buffer-string))))
      ;; Ship again as ready: push, then gh pr ready.
      (with-temp-file (expand-file-name "more.txt" (plist-get task :worktree)) (insert "more\n"))
      (godel-task-update id :last-event "")
      (godel-ship-execute (godel-task-get id) "More" nil nil)
      (should (godel-test-wait done 20))
      (should (equal (plist-get (godel-task-get id) :last-event) "pushed; PR marked ready for review"))
      (should-not (plist-get (godel-task-get id) :pr-draft))
      (should (string-match-p "^pr ready https://github.com/me/repo/pull/5$" (funcall log)))
      (should (eq (godel-test-face-name (godel-board--pr-face (godel-task-get id))) 'godel-state-needs-review))
      ;; And back to draft.
      (godel-task-update id :last-event "")
      (godel-ship-execute (godel-task-get id) nil nil nil t)
      (should (godel-test-wait done 20))
      (should (equal (plist-get (godel-task-get id) :last-event) "pushed; PR converted to draft"))
      (should (string-match-p "^pr ready https://github.com/me/repo/pull/5 --undo$" (funcall log)))
      ;; Ship as draft again while it's a draft: no gh call.
      (delete-file (cdr fake))
      (godel-task-update id :last-event "")
      (godel-ship-execute (godel-task-get id) nil nil nil t)
      (should (godel-test-wait done 20))
      (should (equal (plist-get (godel-task-get id) :last-event) "pushed"))
      (should-not (file-exists-p (cdr fake))))))

(ert-deftest godel-test-ship-check-draft ()
  "The check picks up draft <-> ready changes made on GitHub."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id)))
      (godel-task-update id :pr-url "https://github.com/me/repo/pull/7" :state 'needs-review)
      (let ((godel-gh-program (car (godel-test-fake-gh tmp "unused" "OPEN" t))))
        (godel-ship-check-merged)
        (should (godel-test-wait (lambda () (plist-get (godel-task-get id) :pr-draft)) 5))
        ;; The first look isn't news.
        (should-not (equal (plist-get (godel-task-get id) :last-event) "PR converted to draft")))
      (let ((godel-gh-program (car (godel-test-fake-gh tmp "unused" "OPEN" nil))))
        (godel-ship-check-merged)
        (should (godel-test-wait (lambda () (not (plist-get (godel-task-get id) :pr-draft))) 5))
        (should (equal (plist-get (godel-task-get id) :last-event) "PR marked ready for review")))
      (let ((godel-gh-program (car (godel-test-fake-gh tmp "unused" "OPEN" t))))
        (godel-ship-check-merged)
        (should (godel-test-wait (lambda () (plist-get (godel-task-get id) :pr-draft)) 5))
        (should (equal (plist-get (godel-task-get id) :last-event) "PR converted to draft"))))))

(ert-deftest godel-test-import-worktree ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (outside (expand-file-name "elsewhere/feature-x" tmp))
           (paradev (expand-file-name ".paradev/worktrees/repo/pd" tmp))
           (default-directory repo))
      (call-process "git" nil nil nil "worktree" "add" "-q" "-b" "feature-x" outside "main")
      (call-process "git" nil nil nil "worktree" "add" "-q" "-b" "pd-branch" paradev "main")
      (call-process "git" nil nil nil "worktree" "add" "-q" "--detach"
                    (expand-file-name "detached" tmp) "main")
      (with-temp-file (expand-file-name "new.txt" outside) (insert "x\n"))
      ;; Offered: only the named, non-Paradev one.
      (let ((offered (godel-new-importable-worktrees repo)))
        (should (equal (mapcar #'cdr offered) '("feature-x"))))
      (let* ((task (godel-new-import repo outside "feature-x" "main" "Feature X"))
             (id (plist-get task :id)))
        (should (equal id "feature-x"))
        (should (equal (plist-get task :worktree) (file-name-as-directory outside)))
        (should (equal (plist-get task :branch) "feature-x"))
        (should (plist-get task :session-id))
        (should-not (plist-get task :transcript))
        ;; It has changes, so it's ready for review once checked.
        (should (godel-test-wait (lambda () (eq (plist-get (godel-task-get id) :state) 'needs-review)) 5))
        ;; Not offered again.
        (should-not (godel-new-importable-worktrees repo))
        ;; A fresh session, not a resume, is what R would start.
        (should-not (godel-agent-resumable-p (godel-task-get id)))))))

(ert-deftest godel-test-review-progress-keeps-age ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "t" :repo repo :base "main"))
           (id (plist-get task :id)))
      (godel-task-update id :state 'idle)
      (let ((before (plist-get (godel-task-get id) :updated)))
        (sleep-for 0.01)
        (godel-task-update id :review-activity "Read(x)" :review-status 'running)
        (should (equal (plist-get (godel-task-get id) :updated) before))
        (godel-task-update id :state 'working)
        (should (> (plist-get (godel-task-get id) :updated) before))))))

(ert-deftest godel-test-review-badge ()
  (let ((now (float-time)))
    (should-not (godel-board--review-badge '(:review-status nil)))
    (let ((b (godel-board--review-badge `(:review-status running :review-started ,(- now 18)
                                          :review-activity "Grep(model_params)"))))
      (should (equal (car b) "⟳"))
      (should (string-match-p "\\`reviewing 1[89]s · Grep(model_params)\\'" (cadr b))))
    (should (equal (cadr (godel-board--review-badge
                          '(:review-status done :findings ((:state proposed) (:state proposed) (:state dropped)))))
                   "2 findings"))
    ;; No findings, or failed: shown until the review list is opened.
    (should (equal (cadr (godel-board--review-badge '(:review-status done :findings nil))) "no findings"))
    (should-not (godel-board--review-badge '(:review-status done :findings nil :review-seen t)))
    (should (equal (cadr (godel-board--review-badge '(:review-status failed))) "review failed"))
    (should-not (godel-board--review-badge '(:review-status failed :review-seen t)))
    ;; Findings stay visible until accepted or dropped, even once seen.
    (should (godel-board--review-badge '(:review-status done :review-seen t :findings ((:state proposed)))))))

(ert-deftest godel-test-reviewer-stream-reader ()
  "The real stream reader: JSON lines split across chunks arrive whole."
  (godel-test-with-env
    (let* ((script (expand-file-name "fake.sh" tmp))
           msgs exit-out)
      (with-temp-file script
        (insert "#!/bin/sh\ncat > /dev/null\n"
                "printf 'wrapper text\\n{\"type\":\"system\"}\\n{\"type\":\"assis'\n"
                "sleep 0.2\n"
                "printf 'tant\",\"n\":1}\\n{\"type\":\"result\",\"result\":\"x\"}\\n'\n"))
      (set-file-modes script #o755)
      (godel-reviewer--stream script nil "prompt" process-environment
                              (lambda (m) (push (alist-get 'type m) msgs))
                              (lambda (exit out) (setq exit-out (cons exit out))))
      (should (godel-test-wait (lambda () exit-out) 5))
      (should (equal (reverse msgs) '("system" "assistant" "result")))
      (should (= (car exit-out) 0))
      (should (string-match-p "\"result\":\"x\"" (cdr exit-out))))))

(ert-deftest godel-test-walkthrough-comment-tools ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id))
           (wt (plist-get task :worktree))
           (review (get-buffer-create (godel-agent-buffer-name wt "review")))
           (author (get-buffer-create (godel-agent-buffer-name wt)))
           (ctx nil))
      (unwind-protect
          (cl-letf (((symbol-function 'claude-code-ide-mcp-server-get-session-context)
                     (lambda (&rest _) ctx)))
            ;; Only the task's walkthrough session may call them.
            (setq ctx nil)
            (should-error (godel-reviewer-tool-list-comments))
            (setq ctx (list :project-dir wt :buffer author))
            (should-error (godel-reviewer-tool-add-comment "app.py" 5 5 "x"))
            (setq ctx (list :project-dir wt :buffer review))
            (should (equal (godel-reviewer-tool-list-comments) "No open review comments yet."))
            ;; Relative and absolute paths; end_line defaults to start_line.
            (should (string-match-p "Added a pending comment on app.py:5\\."
                                    (godel-reviewer-tool-add-comment "app.py" 5 nil "Why 20?")))
            (godel-reviewer-tool-add-comment (expand-file-name "new.txt" wt) 1 1 "Needed?")
            (let ((cs (plist-get (godel-task-get id) :comments)))
              (should (equal (mapcar (lambda (c) (list (plist-get c :file) (plist-get c :start)
                                                       (plist-get c :end) (plist-get c :text)
                                                       (plist-get c :state)))
                                     cs)
                             '(("app.py" 5 5 "[walkthrough] Why 20?" pending)
                               ("new.txt" 1 1 "[walkthrough] Needed?" pending)))))
            (should (string-match-p "^- app.py:5 \\[pending\\] \\[walkthrough\\] Why 20\\?"
                                    (godel-reviewer-tool-list-comments)))
            ;; Files outside the worktree, or missing, are refused.
            (should-error (godel-reviewer-tool-add-comment "nope.py" 1 1 "x"))
            (should-error (godel-reviewer-tool-add-comment "/etc/passwd" 1 1 "x")))
        (kill-buffer review)
        (kill-buffer author)))))

(ert-deftest godel-test-walkthrough-tools-registered ()
  (require 'claude-code-ide-mcp-server)
  (let ((names (mapcar (lambda (spec) (plist-get spec :name)) claude-code-ide-mcp-server-tools)))
    (should (member "godel_add_comment" names))
    (should (member "godel_list_comments" names))
    (should (member "godel_show_location" names))))

(ert-deftest godel-test-walkthrough-show-location ()
  "The file opens in the main window at the lines, highlighted; the
walkthrough's window stays selected."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (wt (plist-get task :worktree))
           (review (get-buffer-create (godel-agent-buffer-name wt "review")))
           (ctx (list :project-dir wt :buffer review)))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let ((side (display-buffer-in-side-window review '((side . right)))))
              (select-window side)
              (cl-letf (((symbol-function 'claude-code-ide-mcp-server-get-session-context)
                         (lambda (&rest _) ctx)))
                (should (equal (godel-reviewer-tool-show-location "app.py" 4 5)
                               "Showing app.py:4-5 in the user's Emacs (highlighted).")))
              (sleep-for 0.05)          ; it opens the file on a timer
              (should (eq (selected-window) side))
              (let ((main (window-main-window)))
                (should (equal (buffer-file-name (window-buffer main))
                               (expand-file-name "app.py" wt)))
                (with-current-buffer (window-buffer main)
                  (should (= (line-number-at-pos (window-point main)) 4))
                  (should (equal (buffer-substring-no-properties
                                  (overlay-start godel-reviewer--focus-overlay)
                                  (overlay-end godel-reviewer--focus-overlay))
                                 "def b():\n    return 20\n"))))
              ;; Another caller is refused.
              (setq ctx nil)
              (should-error (godel-reviewer-tool-show-location "app.py" 1 nil))))
        (when (overlayp godel-reviewer--focus-overlay) (delete-overlay godel-reviewer--focus-overlay))
        (kill-buffer review)
        (when-let* ((b (get-file-buffer (expand-file-name "app.py" wt)))) (kill-buffer b))))))

(ert-deftest godel-test-content-window-skips-claude-and-side-windows ()
  "With Claude and magit side by side the main window is internal; the
content window is the one that isn't Claude, never a side window."
  (save-window-excursion
    (delete-other-windows)
    (let* ((claude (get-buffer-create "*claude-code[repo/slug]*"))
           (magit (get-buffer-create "*magit: x*"))
           (side (display-buffer-in-side-window (get-buffer-create "*godel-sidebar*")
                                                '((side . left) (slot . 1)))))
      (unwind-protect
          (progn
            (switch-to-buffer magit)
            (let ((claude-window (split-window (window-main-window) nil 'left)))
              (set-window-buffer claude-window claude)
              (select-window claude-window)
              (should-not (window-live-p (window-main-window)))
              ;; Even with Claude selected, magit's window is chosen.
              (should (eq (window-buffer (godel-agent-content-window)) magit))
              (select-window side)
              (should (eq (window-buffer (godel-agent-content-window t)) magit))
              ;; Only Claude left: strict gives nil, otherwise Claude's window.
              (set-window-buffer (get-buffer-window magit) claude)
              (should-not (godel-agent-content-window t))
              (should (eq (window-buffer (godel-agent-content-window)) claude))))
        (kill-buffer claude) (kill-buffer magit)))))

;;;; Claude's window layout (config.el), against the real claude-code-ide

(defconst godel-test-claude-advice
  '((claude-code-ide--display-buffer-in-side-window . my/claude-code-display-advice)
    (claude-code-ide--toggle-existing-window . my/claude-code-hide-advice))
  "The advice config.el's layout block installs: (FUNCTION . ADVICE).")

(defun godel-test-load-claude-layout ()
  "Evaluate config.el's Claude layout block; return nil if it can't be found."
  (let* ((file (expand-file-name "../../../config.el"
                                 (file-name-directory (or load-file-name buffer-file-name
                                                          (locate-library "godel-test")))))
         (text (and (file-readable-p file)
                    (with-temp-buffer (insert-file-contents file) (buffer-string))))
         (a (and text (string-match ";; Claude code\n;; Layout" text)))
         (b (and text (string-match "(use-package! claude-code-ide" text))))
    (when (and a b (< a b))
      (with-temp-buffer
        (insert (substring text a b))
        (goto-char (point-min))
        (condition-case nil
            (while t (eval (read (current-buffer)) t))
          (end-of-file nil)))
      (fboundp 'my/claude-code-display-advice))))

(defmacro godel-test-with-claude-layout (&rest body)
  "Run BODY in a 160-column frame with the sidebar and magit windows set up and
config.el's Claude layout active.  Binds `claude' (a task's session buffer),
`review' (an extra instance of it), `other' (a session outside any godel task)
and `magit' (the magit window).
Only buffers BODY creates are killed afterwards, and the advice is removed only
if this test installed it: run from a live Emacs, it leaves your sessions and
your own layout alone."
  (declare (indent 0))
  `(progn
     (skip-unless (require 'claude-code-ide nil t))
     (skip-unless (not (cl-some #'get-buffer
                                '("*claude-code[repo/slug]*" "*claude-code[repo/slug:review]*"
                                  "*claude-code[plain]*" "*magit: x*" "*godel-sidebar*"
                                  "diff-A" "diff-B"))))
     (let* ((width (frame-width))
            (existing (buffer-list))
            (had-advice (cl-every (lambda (pair) (advice-member-p (cdr pair) (car pair)))
                                  godel-test-claude-advice))
            (claude-code-ide-use-side-window t)
            (claude-code-ide-window-side 'right)
            (claude-code-ide-window-width 30)
            (claude-code-ide-focus-on-open nil)
            claude review other magit)
       (unwind-protect
           (save-window-excursion
             (skip-unless (godel-test-load-claude-layout))
             (let ((my/claude-code-main-fraction 0.6)
                   (my/claude-code-side-widths '(30 20)))
               (setq claude (get-buffer-create "*claude-code[repo/slug]*")
                     review (get-buffer-create "*claude-code[repo/slug:review]*")
                     other (get-buffer-create "*claude-code[plain]*"))
               (set-frame-width nil 160)
               (delete-other-windows)
               (set-window-parameter
                (display-buffer-in-side-window (get-buffer-create "*godel-sidebar*")
                                               '((side . left) (slot . 1) (window-width . 12)))
                'no-delete-other-windows t)
               (switch-to-buffer (get-buffer-create "*magit: x*"))
               (setq magit (selected-window))
               ,@body))
         (set-frame-width nil width)
         (unless had-advice
           (dolist (pair godel-test-claude-advice)
             (when (fboundp (car pair)) (advice-remove (car pair) (cdr pair)))))
         (dolist (b (buffer-list))
           (unless (or (memq b existing)
                       (string-match-p "\\` \\*\\(Minibuf\\|Echo Area\\)" (buffer-name b)))
             (kill-buffer b)))))))

(defun godel-test-frame-columns ()
  "Columns the windows span.  (`frame-width' doesn't follow `set-frame-width'
in a batch Emacs.)"
  (window-total-width (frame-root-window)))

(defun godel-test-layout-names ()
  "Buffer names of the live windows, left to right."
  (mapcar (lambda (w) (buffer-name (window-buffer w)))
          (sort (window-list nil 'no-minibuf)
                (lambda (a b) (< (window-left-column a) (window-left-column b))))))

(defun godel-test-near-p (actual expected)
  (<= (abs (- actual expected)) 1))

(ert-deftest godel-test-claude-layout-beside-sidebar ()
  "The session opens between the sidebar and magit, takes 60% of the main
area, is an ordinary window (`SPC w o' closes it, leaving magit and the sidebar)."
  (godel-test-with-claude-layout
    (claude-code-ide--display-buffer-in-side-window claude)
    (should (equal (godel-test-layout-names)
                   '("*godel-sidebar*" "*claude-code[repo/slug]*" "*magit: x*")))
    (let ((window (get-buffer-window claude)))
      (should-not (window-parameter window 'window-side))
      (should-not (window-parameter window 'no-delete-other-windows))
      (should (window-dedicated-p window))
      (should (godel-test-near-p (window-total-width window)
                                 (round (* 0.6 (- (godel-test-frame-columns) 12))))))
    ;; `delete-other-windows' closes Claude's window, but not the sidebar's,
    ;; which godel protects.
    (select-window magit)
    (delete-other-windows)
    (should (equal (godel-test-layout-names) '("*godel-sidebar*" "*magit: x*")))))

(ert-deftest godel-test-claude-layout-only-for-task-sessions ()
  "A session outside a godel task, and an extra instance, keep claude-code-ide's
right side window."
  (godel-test-with-claude-layout
    ;; (One at a time: without sessions they share a side-window slot.)
    (dolist (buffer (list other review))
      (claude-code-ide--display-buffer-in-side-window buffer)
      (should (eq (window-parameter (get-buffer-window buffer) 'window-side) 'right))
      (should (equal (car (last (godel-test-layout-names))) (buffer-name buffer)))
      (should-not (get-buffer-window claude)))))

(ert-deftest godel-test-claude-shown-again-keeps-its-window ()
  "Showing a session that's already visible (`SPC c l') leaves its window as it
was: still 60%, still closable by `delete-other-windows'."
  (godel-test-with-claude-layout
    (claude-code-ide--display-buffer-in-side-window claude)
    (let* ((window (get-buffer-window claude))
           (width (window-total-width window)))
      (claude-code-ide--display-buffer-in-side-window claude)
      (should (eq (get-buffer-window claude) window))
      (should (= (window-total-width window) width))
      (should-not (window-parameter window 'no-delete-other-windows))
      (select-window window)
      (delete-other-windows)
      (should (equal (godel-test-layout-names)
                     '("*godel-sidebar*" "*claude-code[repo/slug]*"))))))

(ert-deftest godel-test-claude-hide-when-it-is-the-main-window ()
  "After `SPC w o' from Claude's window it is the frame's main window, which
`delete-window' refuses to delete: `SPC c t' must hide it anyway, and show it again."
  (godel-test-with-claude-layout
    (claude-code-ide--display-buffer-in-side-window claude)
    (select-window (get-buffer-window claude))
    (delete-other-windows)
    (should-not (window-deletable-p (get-buffer-window claude)))
    (let ((session (make-claude-code-ide-mcp-session :buffer claude)))
      (claude-code-ide--toggle-existing-window session)
      (should-not (get-buffer-window claude))
      (should (= (length (cl-remove "*godel-sidebar*" (godel-test-layout-names)
                                    :test #'equal))
                 1))
      (claude-code-ide--toggle-existing-window session)
      (should (get-buffer-window claude))
      (should (= (length (window-list nil 'no-minibuf)) 3)))))

(ert-deftest godel-test-claude-width-toggle ()
  "The toggle resizes the task session to 40% / 60% of the main area and the
side window to its fixed widths, and leaves a session that is the whole main
area alone."
  (godel-test-with-claude-layout
    (claude-code-ide--display-buffer-in-side-window claude)
    (claude-code-ide--display-buffer-in-side-window review)
    (let ((review-window (get-buffer-window review))
          (main-area (- (godel-test-frame-columns) 12
                        (window-total-width (get-buffer-window review))))
          (inhibit-message t))
      (should (window-parameter review-window 'window-side))
      (should (= (window-body-width review-window) 30))
      (claude-code-ide-toggle-width)
      (should (= my/claude-code-main-fraction 0.4))
      (should (= (window-body-width review-window) 20))
      (should (godel-test-near-p (window-total-width (get-buffer-window claude))
                                 (round (* 0.4 (- (godel-test-frame-columns) 12
                                                  (window-total-width review-window))))))
      (claude-code-ide-toggle-width)
      (should (= (window-body-width review-window) 30))
      (should (godel-test-near-p (window-total-width (get-buffer-window claude))
                                 (round (* 0.6 main-area))))
      ;; Magit's window closed: Claude is the main window, so it fills what the
      ;; side windows leave (it isn't resized to a fraction of that).
      (delete-window magit)
      (should (eq (get-buffer-window claude) (window-main-window)))
      (claude-code-ide-toggle-width)
      (should (= (window-total-width (get-buffer-window claude))
                 (- (godel-test-frame-columns) 12 (window-total-width review-window))))
      (claude-code-ide-toggle-width)
      (should (= (window-total-width (get-buffer-window claude))
                 (- (godel-test-frame-columns) 12 (window-total-width review-window)))))))

(ert-deftest godel-test-claude-ide-diff-from-either-window ()
  "claude-code-ide's diff (openDiff: delete the side windows, ediff, show Claude
again, restore the layout on quit) gives the same result whether it starts in
magit's window or in Claude's: ediff gets the frame, Claude comes back to its
left with a small share, and quitting restores the original layout."
  (dolist (start '(magit claude))
    (godel-test-with-claude-layout
      (claude-code-ide--display-buffer-in-side-window claude)
      (let* ((a (get-buffer-create "diff-A")) (b (get-buffer-create "diff-B"))
             (before (godel-test-layout-names))
             (columns (godel-test-frame-columns))
             (saved nil)
             (inhibit-message t))
        (with-current-buffer a (insert "one\ntwo\nthree\n"))
        (with-current-buffer b (insert "one\n2\nthree\n"))
        (select-window (if (eq start 'claude) (get-buffer-window claude) magit))
        (setq saved (current-window-configuration))
        (let ((ediff-window-setup-function 'ediff-setup-windows-plain)
              (ediff-split-window-function 'split-window-horizontally))
          (dolist (w (window-list))
            (when (window-parameter w 'window-side) (delete-window w)))
          (ediff-buffers a b))
        ;; ediff has the whole frame.
        (should (my/claude-code-ediff-up-p))
        (should (>= (+ (window-total-width (get-buffer-window a))
                       (window-total-width (get-buffer-window b)))
                    (- columns 2)))
        ;; claude-code-ide shows Claude again once ediff is up.
        (claude-code-ide--display-buffer-in-side-window claude)
        (let ((claude-window (get-buffer-window claude))
              (window-a (get-buffer-window a))
              (window-b (get-buffer-window b)))
          (should (equal (car (godel-test-layout-names)) "*claude-code[repo/slug]*"))
          (should-not (window-parameter claude-window 'window-side))
          (should-not (window-parameter claude-window 'no-delete-other-windows))
          (should (< (window-left-column claude-window) (window-left-column window-a)))
          (should (< (window-left-column claude-window) (window-left-column window-b)))
          ;; The diff keeps most of the frame: Claude has the small share, each
          ;; diff window more than Claude, and together they have more than 60%.
          (should (godel-test-near-p (window-total-width claude-window)
                                     (round (* my/claude-code-ediff-fraction columns))))
          (should (> (window-total-width window-a) (window-total-width claude-window)))
          (should (> (window-total-width window-b) (window-total-width claude-window)))
          (should (>= (+ (window-total-width window-a) (window-total-width window-b))
                      (round (* 0.6 columns)))))
        (set-window-configuration saved)
        (should (equal (godel-test-layout-names) before))))))

(ert-deftest godel-test-board-select-content-window ()
  "The diff from the sidebar must never take Claude's window: with magit's
window gone, Claude's is split and the new half is used.  Visit (non-strict)
may take Claude's, since it splits it back in afterwards."
  (save-window-excursion
    (delete-other-windows)
    (let* ((claude (get-buffer-create "*claude-code[repo/slug]*"))
           (side (display-buffer-in-side-window (get-buffer-create "*godel-sidebar*")
                                                '((side . left) (slot . 1)))))
      (unwind-protect
          (progn
            (switch-to-buffer claude)
            (let ((claude-window (get-buffer-window claude)))
              (set-window-dedicated-p claude-window t)
              (select-window side)
              ;; Non-strict: Claude's own window, undedicated.
              (godel-board--select-content-window)
              (should (eq (selected-window) claude-window))
              (should-not (window-dedicated-p claude-window))
              ;; Strict: a new window, not Claude's.
              (select-window side)
              (godel-board--select-content-window t)
              (should-not (eq (selected-window) claude-window))
              (should (eq (window-buffer claude-window) claude))
              (should-not (window-dedicated-p (selected-window)))
              (should-not (window-parameter (selected-window) 'window-side))
              ;; With a non-Claude window around, strict just uses it.
              (switch-to-buffer (get-buffer-create "*magit: x*"))
              (select-window side)
              (let ((before (length (window-list))))
                (godel-board--select-content-window t)
                (should (equal (buffer-name (window-buffer (selected-window))) "*magit: x*"))
                (should (= (length (window-list)) before)))))
        (kill-buffer claude)
        (when-let* ((b (get-buffer "*magit: x*"))) (kill-buffer b))))))

(ert-deftest godel-test-walkthrough-show-location-beside-claude ()
  "The file opens in the window that isn't Claude, and Claude's window keeps
the conversation."
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (wt (plist-get task :worktree))
           (review (get-buffer-create (godel-agent-buffer-name wt "review")))
           (author (get-buffer-create (godel-agent-buffer-name wt)))
           (ctx (list :project-dir wt :buffer review)))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer (get-buffer-create "*magit: x*"))
            (let ((author-window (split-window (window-main-window) nil 'left))
                  (side (display-buffer-in-side-window review '((side . right)))))
              (set-window-buffer author-window author)
              (select-window side)
              (cl-letf (((symbol-function 'claude-code-ide-mcp-server-get-session-context)
                         (lambda (&rest _) ctx)))
                (godel-reviewer-tool-show-location "app.py" 4 5))
              (sleep-for 0.05)
              (should (eq (window-buffer author-window) author))
              (should (eq (selected-window) side))
              (should (equal (buffer-file-name
                              (window-buffer (godel-agent-content-window t)))
                             (expand-file-name "app.py" wt)))))
        (when (overlayp godel-reviewer--focus-overlay) (delete-overlay godel-reviewer--focus-overlay))
        (dolist (b (list review author (get-buffer "*magit: x*"))) (when b (kill-buffer b)))
        (when-let* ((b (get-file-buffer (expand-file-name "app.py" wt)))) (kill-buffer b))))))

(ert-deftest godel-test-review-history ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp)))
      (should-error (godel-review-history task) :type 'user-error)
      (godel-task-update (plist-get task :id) :review-rounds
                         (list (list :round 1 :time 0 :text "first") (list :round 2 :time 1 :text "second")))
      (save-window-excursion
        (godel-review-history (godel-task-get (plist-get task :id)))
        (with-current-buffer (format "*godel-review-history: %s*" (plist-get task :id))
          (should (godel-test-text-order-p "Round 2" "second" "Round 1" "first"))
          (kill-buffer))))))

(ert-deftest godel-test-board-comment-summary ()
  (should-not (godel-board--comment-summary '(:comments nil)))
  (should (equal (godel-board--comment-summary
                  '(:comments ((:state pending) (:state sent) (:state pending) (:state outdated))))
                 "2 pending, 1 sent, 1 outdated")))

(ert-deftest godel-test-delete-comment-at-point ()
  (godel-test-with-env
    (let* ((task (godel-test-review-task tmp))
           (id (plist-get task :id)))
      (godel-review-add-comment id "app.py" 5 5 "Why 20?")
      (godel-review-add-comment id "app.py" 2 2 "Fine")
      (godel-review-update-comment id 2 :state 'addressed)
      ;; In the diff, on the commented line.
      (save-window-excursion
        (godel-review-diff task)
        (let ((buffer (current-buffer)))
          (unwind-protect
              (with-current-buffer buffer
                (goto-char (point-min))
                (search-forward "+    return 20")
                (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                  (godel-review-delete-comment-at-point))
                (should (equal (mapcar (lambda (c) (plist-get c :id))
                                       (plist-get (godel-task-get id) :comments))
                               '(2)))
                ;; A line without a comment.
                (goto-char (point-min))
                (search-forward "+def c")
                (should-error (godel-review-delete-comment-at-point) :type 'user-error))
            (kill-buffer buffer))))
      ;; In the file: an addressed comment can be deleted too; "no" keeps it.
      (let ((buffer (find-file-noselect (expand-file-name "app.py" (plist-get task :worktree)))))
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (forward-line 1)
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
                (godel-review-delete-comment-at-point))
              (should (= (length (plist-get (godel-task-get id) :comments)) 1))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (godel-review-delete-comment-at-point))
              (should-not (plist-get (godel-task-get id) :comments)))
          (kill-buffer buffer))))))

(ert-deftest godel-test-comment-text-faces-complete ()
  "Every character of a comment's display text ends its face list with
`default', so nothing is borrowed from the line it's drawn in front of."
  (let ((text (godel-review--comment-string
               '(:text "is the model_params right\nsecond line" :state pending :start 271 :end 271))))
    (should (string-match-p "▍ is the model_params right" text))
    (should (string-match-p "pending · line 271" text))
    (dotimes (i (length text))
      (let ((face (get-text-property i 'face text)))
        (should (consp face))
        (should (eq (car (last face)) 'default))))))

(ert-deftest godel-test-compose-quote-not-submitted ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (godel-compose--last-cancelled nil)
           got
           (buffer (godel-compose repo 'comment "c" (lambda (text) (setq got text))
                                  nil "> quoted line")))
      (with-current-buffer buffer
        ;; Shown above the text, not part of it.
        (should (equal (buffer-string) ""))
        (should (cl-some (lambda (ov) (string-match-p "quoted line" (or (overlay-get ov 'before-string) "")))
                         (overlays-in (point-min) (point-max))))
        ;; Typing works from where the buffer starts.
        (insert "My comment")
        (should (cl-some (lambda (ov) (= (overlay-start ov) (point-min)))
                         (overlays-in (point-min) (point-max))))
        (godel-compose-submit))
      (should (equal got "My comment")))))

(ert-deftest godel-test-reconcile-marks-dead-sessions-stopped ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (working (godel-task-create :title "a" :repo repo :base "main"))
           (review (godel-task-create :title "b" :repo repo :base "main")))
      (godel-task-update (plist-get working :id) :state 'working)
      (godel-task-update (plist-get review :id) :state 'needs-review)
      (godel--reconcile)
      (should (eq (plist-get (godel-task-get (plist-get working :id)) :state) 'stopped))
      (should (eq (plist-get (godel-task-get (plist-get review :id)) :state) 'needs-review)))))

(ert-deftest godel-test-status-event-never-signals ()
  (should-not (godel-status-event "Stop" "not base64 !!!")))

;;;; Worktree config

(ert-deftest godel-test-toml-subset ()
  (godel-test-with-env
    (let ((repo (godel-test-repo (expand-file-name "repo" tmp))))
      (with-temp-file (expand-file-name ".godel.toml" repo)
        (insert "# godel config\nsetup = [\"npm ci\", \"cp ../.env .env\"]  # deps\n"
                "teardown = \"echo bye\"\nignored line\n"))
      (should (equal (godel-worktree--commands repo 'setup) '("npm ci" "cp ../.env .env")))
      (should (equal (godel-worktree--commands repo 'teardown) '("echo bye")))
      (should (null (godel-worktree--commands repo 'other))))))

(ert-deftest godel-test-worktree-create-and-remove ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "wt" :repo repo :base "main"))
           (result 'pending))
      (with-temp-file (expand-file-name ".godel.toml" repo)
        (insert "setup = \"touch setup-ran\"\n"))
      (godel-worktree-create task (lambda (err) (setq result err)))
      (should (godel-test-wait (lambda () (not (eq result 'pending)))))
      (should (null result))
      (should (file-exists-p (expand-file-name "setup-ran" (plist-get task :worktree))))
      (setq result 'pending)
      (let (counts)
        (godel-worktree-check task (lambda (c) (setq counts c)))
        (should (godel-test-wait (lambda () counts)))
        (should (equal counts '(1 . 0))))   ; setup-ran is untracked
      (godel-worktree-remove task (lambda (err) (setq result err)))
      (should (godel-test-wait (lambda () (not (eq result 'pending)))))
      (should (null result))
      (should-not (file-directory-p (plist-get task :worktree)))
      ;; Branch kept.
      (should (godel-task--git repo "rev-parse" "--verify" (plist-get task :branch))))))

;;;; Compose

(ert-deftest godel-test-compose-remaps ()
  (should (eq (lookup-key godel-compose-keys-mode-map [remap evil-save-and-close]) #'godel-compose-submit))
  (should (eq (lookup-key godel-compose-keys-mode-map [remap evil-save-modified-and-close]) #'godel-compose-submit))
  (should (eq (lookup-key godel-compose-keys-mode-map [remap evil-quit]) #'godel-compose-cancel)))

(ert-deftest godel-test-compose-submit-and-cancel ()
  (godel-test-with-env
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (godel-compose--last-cancelled nil)
           got buffer file)
      (setq buffer (godel-compose repo 'task "test" (lambda (text) (setq got text))))
      (with-current-buffer buffer
        (setq file buffer-file-name)
        (insert "Title line\n\nBody")
        (godel-compose-submit))
      (should (equal got "Title line\n\nBody"))
      (should-not (buffer-live-p buffer))
      (should-not (file-exists-p file))
      ;; Cancel keeps the draft and offers it next time.
      (setq buffer (godel-compose repo 'task "test" #'ignore))
      (with-current-buffer buffer
        (setq file buffer-file-name)
        (insert "draft text")
        (godel-compose-cancel))
      (should (file-exists-p file))
      (should (equal (alist-get 'task godel-compose--last-cancelled) file))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (setq buffer (godel-compose repo 'task "test" #'ignore)))
      (with-current-buffer buffer
        (should (equal (string-trim (buffer-string)) "draft text"))
        (godel-compose--close nil)))))

(ert-deftest godel-test-new-split ()
  (should (equal (godel-new--split "Fix login\n\nDetails here") '("Fix login" . "Details here")))
  (should (equal (godel-new--split "# Just a title") '("Just a title" . "# Just a title")))
  (should (string-match-p "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'"
                          (godel-new--uuid))))

(ert-deftest godel-test-new-task-effort-for-every-model ()
  "-e is never greyed out (Not Diamond included); new tasks default to medium."
  ;; `transient--suffix-predicate' is the condition transient itself checks for
  ;; a suffix's `:inapt-if' (and `:if', ...).  -e has none, whatever the model.
  ;; A plist read of the spec would pass even with the condition put back.
  (should-not (transient--suffix-predicate (transient-get-suffix 'godel-new-task-menu "-e")))
  (godel-test-with-env
    (let ((godel-new--repo (godel-test-repo (expand-file-name "repo" tmp))))
      (should (member "--effort=medium"
                      (funcall (oref (get 'godel-new-task-menu 'transient--prefix) default-value))))
      (should (member "--model=notdiamond[1m]"
                      (funcall (oref (get 'godel-new-task-menu 'transient--prefix) default-value)))))))

;;;; TUI

(ert-deftest godel-test-tui-predicates ()
  (let ((trust " Accessing workspace:\n ❯ No, exit\n   Yes, I trust this folder\n")
        (trust-yes " Accessing workspace:\n   No, exit\n ❯ Yes, I trust this folder\n")
        (input "────\n❯ Try \"refactor <filepath>\"\n────\n  ⏵⏵ auto mode on\n")
        (picker "   Select model\n   ❯ 6. Not Diamond ✔          Routes\n")
        ;; Real ghostel text: a no-break space after the arrow.
        (input-nbsp "────\n❯\u00a0Try \"how do I log an error?\"\n────\n"))
    (should (godel-tui-input-ready-p input-nbsp))
    (should (godel-tui-trust-dialog-p trust))
    (should-not (godel-tui-trust-cursor-on-yes-p trust))
    (should (godel-tui-trust-cursor-on-yes-p trust-yes))
    (should (godel-tui-input-ready-p input))
    (should-not (godel-tui-input-ready-p trust))
    (should-not (godel-tui-input-ready-p picker))))

(provide 'godel-test)
;;; godel-test.el ends here
