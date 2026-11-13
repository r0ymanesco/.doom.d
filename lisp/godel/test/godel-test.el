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
           ;; Nothing listening, so the direct check fails and the CLI is used.
           (godel-proxy-address (cons "127.0.0.1" (godel-test-free-port)))
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
                (should (string-match-p "Why 20\\?" (overlay-get (car godel-comment--overlays) 'after-string)))
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
