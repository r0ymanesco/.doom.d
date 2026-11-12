;;; godel-test.el --- Tests for godel -*- lexical-binding: t -*-

;;; Commentary:
;; Run with test/run-tests.sh.  Uses temporary git repos; never touches
;; ~/.godel or a real Claude session.

;;; Code:

(require 'ert)
(require 'godel)

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
          (godel-models--cache nil))
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
    "NOTDIAMOND_ROUTING=off" "PATH=/usr/bin:/bin" "HOME=/tmp"))

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
    (dolist (var godel-pin-variables)
      (should-not (godel-test-env-value (plist-get launch :env) var))
      (should-not (cl-some (lambda (e) (string-prefix-p (concat var "=") e))
                           (plist-get launch :env))))
    (should (null (plist-get launch :args)))
    (should (null (plist-get launch :pinned)))
    (should (equal (plist-get launch :expect) "notdiamond[1m]"))
    (should (equal (godel-test-env-value (plist-get launch :env) "PATH") "/usr/bin:/bin"))))

(ert-deftest godel-test-launch-env-pinned ()
  (let* ((launch (godel-models-launch-env godel-test-sonnet "high" godel-test-inherited))
         (env (plist-get launch :env)))
    (should (equal (godel-test-env-value env "NOTDIAMOND_RESEARCH_MODEL_PIN") "sonnet"))
    (should (equal (godel-test-env-value env "NOTDIAMOND_RESEARCH_EFFORT_PIN") "high"))
    (should (equal (godel-test-env-value env "NOTDIAMOND_ROUTING") "off"))
    (should (equal (plist-get launch :args) '("--effort" "high")))
    (should (plist-get launch :pinned))
    (should (equal (plist-get launch :expect) "claude-sonnet-5-5"))))

(defun godel-test-child-env (env)
  "Environment lines a real child process sees with ENV."
  (let ((process-environment env)
        (default-directory temporary-file-directory))
    (with-temp-buffer
      (call-process "env" nil t nil)
      (split-string (buffer-string) "\n" t))))

(ert-deftest godel-test-launch-env-child-process ()
  "A real child process: Not Diamond sees no pins even if Emacs has them."
  (let* ((inherited (append godel-test-inherited process-environment))
         (router (godel-test-child-env
                  (plist-get (godel-models-launch-env godel-test-router "high" inherited) :env)))
         (pinned (godel-test-child-env
                  (plist-get (godel-models-launch-env godel-test-sonnet "low" inherited) :env))))
    (should-not (cl-some (lambda (l) (string-prefix-p "NOTDIAMOND_" l)) router))
    (should (member "NOTDIAMOND_RESEARCH_MODEL_PIN=sonnet" pinned))
    (should (member "NOTDIAMOND_RESEARCH_EFFORT_PIN=low" pinned))
    (should (member "NOTDIAMOND_ROUTING=off" pinned))))

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
        (claude-sonnet '(("claude" "--model" "sonnet" "--effort" "low") . ("NOTDIAMOND_ROUTING=off"))))
    (should (car (godel-agent-check-launch (list wrapper claude-router) godel-test-router)))
    ;; A pin anywhere in the tree fails a Not Diamond session.
    (should-not (car (godel-agent-check-launch
                      (list (cons (car wrapper) '("NOTDIAMOND_RESEARCH_MODEL_PIN=opus")) claude-router)
                      godel-test-router)))
    (should-not (car (godel-agent-check-launch (list wrapper claude-sonnet) godel-test-router)))
    (should (car (godel-agent-check-launch (list wrapper claude-sonnet) godel-test-sonnet)))
    (should-not (car (godel-agent-check-launch (list wrapper claude-router) godel-test-sonnet)))
    (should-not (car (godel-agent-check-launch (list wrapper) godel-test-router)))))

(ert-deftest godel-test-same-model ()
  (should (godel-status-same-model-p "claude-sonnet-5-5[1m]" "claude-sonnet-5-5"))
  (should (godel-status-same-model-p "notdiamond[1m]" "notdiamond[1m]"))
  (should-not (godel-status-same-model-p "claude-opus-5-5" "notdiamond[1m]")))

(ert-deftest godel-test-proxy-guard-starts-clean-proxy ()
  "No proxy running: godel starts one, and the pin variables don't reach it."
  (godel-test-with-env
    (let* ((state (expand-file-name "proxy-started" tmp))
           (env-log (expand-file-name "start-env" tmp))
           (fake (expand-file-name "notdiamond" tmp))
           (godel-notdiamond-program fake)
           (process-environment (append '("NOTDIAMOND_ROUTING=off"
                                          "NOTDIAMOND_RESEARCH_MODEL_PIN=opus")
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
        (should-not (string-match-p "NOTDIAMOND_" (buffer-string))))
      ;; Proxy already running: no second start.
      (delete-file env-log)
      (setq result 'pending)
      (godel-models-ensure-proxy (lambda (ok &optional _) (setq result ok)))
      (should (godel-test-wait (lambda () (not (eq result 'pending)))))
      (should (eq result t))
      (should-not (file-exists-p env-log)))))

(ert-deftest godel-test-pin-check-result ()
  (should (eq (car (godel-models--check-result '("--settings" "{}" "--model" "haiku")
                                               '("--model" "notdiamond[1m]") "haiku"))
              'ok))
  (should (eq (car (godel-models--check-result '("--model" "notdiamond[1m]")
                                               '("--model" "notdiamond[1m]") "haiku"))
              'broken))
  (should (eq (car (godel-models--check-result '("--model" "haiku") '("--settings" "{}") "haiku"))
              'broken))
  (should (eq (car (godel-models--check-result nil '("--model" "notdiamond[1m]") "haiku"))
              'unknown)))

(defun godel-test-fake-wrapper (dir honour-pin)
  "Write fake notdiamond and notdiamond-claude programs into DIR.
The fake wrapper starts claudeBinaryPath from $NOTDIAMOND_CONFIG with
--model set like the real one (or ignoring the pin unless HONOUR-PIN)."
  (let ((nd (expand-file-name "notdiamond" dir))
        (wrapper (expand-file-name "notdiamond-claude" dir)))
    (with-temp-file nd
      (insert "#!/bin/sh\n[ \"$1\" = status ] && echo '{\"ok\":true}'\nexit 0\n"))
    (with-temp-file wrapper
      (insert "#!/bin/sh\n"
              "bin=$(python3 -I -c 'import json,os,sys;print(json.load(open(os.environ[\"NOTDIAMOND_CONFIG\"]))[\"claudeBinaryPath\"])')\n"
              "model='notdiamond[1m]'\n"
              (if honour-pin "[ -n \"$NOTDIAMOND_RESEARCH_MODEL_PIN\" ] && model=$NOTDIAMOND_RESEARCH_MODEL_PIN\n" "")
              "exec \"$bin\" --settings '{}' --model \"$model\"\n"))
    (set-file-modes nd #o755)
    (set-file-modes wrapper #o755)
    (cons nd wrapper)))

(defmacro godel-test-with-fake-wrapper (honour-pin &rest body)
  (declare (indent 1))
  `(godel-test-with-env
     (let* ((fakes (godel-test-fake-wrapper tmp ,honour-pin))
            (godel-notdiamond-program (car fakes))
            (godel-claude-program (cdr fakes))
            (godel-claude-binary "/bin/true")
            (source (expand-file-name "nd-config.json" tmp))
            (process-environment (cons (concat "NOTDIAMOND_CONFIG=" source) process-environment))
            (godel-models-pin-status nil)
            (result nil))
       (with-temp-file source (insert "{\"server\":{\"port\":8082},\"credential\":{\"apiKey\":null}}"))
       ,@body)))

(ert-deftest godel-test-pin-check-ok ()
  (godel-test-with-fake-wrapper t
    (godel-models-check-pin (lambda (status) (setq result status)))
    (should (godel-test-wait (lambda () result) 20))
    (should (eq (car result) 'ok))
    (should-not (godel-models-pin-broken-p))))

(ert-deftest godel-test-pin-check-broken ()
  (godel-test-with-fake-wrapper nil
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (godel-models-check-pin (lambda (status) (setq result status)))
      (should (godel-test-wait (lambda () result) 20)))
    (should (eq (car result) 'broken))
    (should (godel-models-pin-broken-p))
    ;; Pinned models are unavailable in the picker; Not Diamond isn't.
    (should (godel-new--unavailable-p godel-test-sonnet))
    (should-not (godel-new--unavailable-p godel-test-router))
    (should (string-match-p "unavailable" (godel-new--model-label godel-test-sonnet)))
    ;; And a pinned task isn't launched.
    (let* ((repo (godel-test-repo (expand-file-name "repo" tmp)))
           (task (godel-task-create :title "p" :repo repo :base "main" :model "sonnet")))
      (should-error (godel-agent-start task) :type 'user-error)
      (should (eq (plist-get (godel-task-get (plist-get task :id)) :state) 'stopped)))))

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
