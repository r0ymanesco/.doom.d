;;; godel-models.el --- Model list, launch environment and proxy guard -*- lexical-binding: t -*-

;;; Commentary:
;; - The model list comes from the Agent SDK `initialize' control request,
;;   sent to `godel-claude-program' in stream-json mode.  It is cached in
;;   `godel-models-cache-file' and refreshed when Claude Code or notdiamond
;;   change version.
;; - `godel-models-launch-env' is the one place that decides the environment
;;   and extra flags for a launch.  Not Diamond (the router) launches never
;;   carry the pin variables; every other model is pinned.
;; - `godel-models-ensure-proxy' makes sure the notdiamond proxy is running
;;   and was not started by a pinned launch (which would turn routing off for
;;   every session).
;;
;; See PLAN.md, "Model and effort".

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'godel-task)

(defcustom godel-claude-program "notdiamond-claude"
  "Program used to start Claude sessions."
  :group 'godel
  :type 'string)

(defcustom godel-claude-binary (expand-file-name "~/.local/bin/claude")
  "The real Claude Code binary, used only to read its version.
Not `claude' on PATH, which may be another tool's wrapper."
  :group 'godel
  :type 'file)

(defcustom godel-notdiamond-program
  (or (executable-find "notdiamond")
      (expand-file-name "~/.npm-global/bin/notdiamond"))
  "The notdiamond CLI, used for the proxy status and start commands."
  :group 'godel
  :type 'file)

(defcustom godel-model-options
  '((:value "default" :name "Default (recommended)"
     :description "Opus 5.5 · Best for everyday, complex tasks"
     :efforts ("low" "medium" "high" "xhigh" "max") :resolved "claude-opus-5-5")
    (:value "opus" :name "Opus"
     :description "Opus 5.5 · Best for everyday, complex tasks"
     :efforts ("low" "medium" "high" "xhigh" "max") :resolved "claude-opus-5-5")
    (:value "claude-fable-5-1[1m]" :name "Fable"
     :description "Fable 5.1 · Most capable for your hardest and longest-running tasks"
     :efforts ("low" "medium" "high" "xhigh" "max") :resolved "claude-fable-5-1")
    (:value "sonnet" :name "Sonnet"
     :description "Sonnet 5.5 · Efficient for routine tasks"
     :efforts ("low" "medium" "high" "xhigh" "max") :resolved "claude-sonnet-5-5")
    (:value "haiku" :name "Haiku"
     :description "Haiku 5.5 · Fastest for quick answers"
     :efforts ("low" "medium" "high" "xhigh" "max") :resolved "claude-haiku-5-5")
    (:value "notdiamond[1m]" :name "Not Diamond"
     :description "Routes each turn to the best model and effort"
     :efforts ("low" "medium" "high" "xhigh" "max") :resolved "notdiamond[1m]"))
  "Fallback model list, used until the `initialize' request has answered.
Each entry is a plist with :value, :name, :description, :efforts and
:resolved, as returned by `godel-models-parse-initialize'."
  :group 'godel
  :type '(repeat plist))

(defcustom godel-default-model "notdiamond[1m]"
  "Model value for new tasks.  The default is the Not Diamond router."
  :group 'godel
  :type 'string)

(defcustom godel-default-effort "high"
  "Effort for new tasks on a pinned model (the router picks its own)."
  :group 'godel
  :type 'string)

(defconst godel-pin-variables
  '("NOTDIAMOND_RESEARCH_MODEL_PIN" "NOTDIAMOND_RESEARCH_EFFORT_PIN" "NOTDIAMOND_ROUTING")
  "Environment variables that pin a notdiamond session to one model.")

(defun godel-models-cache-file ()
  (expand-file-name "models.eld" godel-directory))

;;;; Model list

(defvar godel-models-pin-status)

(defvar godel-models--cache nil
  "Plist (:models LIST :versions STRING :fetched TIME), or nil.")

(defun godel-models--load-cache ()
  (unless godel-models--cache
    (setq godel-models--cache (godel-task--read-file (godel-models-cache-file)))
    (unless godel-models-pin-status
      (setq godel-models-pin-status (plist-get godel-models--cache :pin-status))))
  godel-models--cache)

(defun godel-models ()
  "Return the current model list (cached `initialize' result or fallback)."
  (or (plist-get (godel-models--load-cache) :models) godel-model-options))

(defun godel-models-find (value)
  "Return the model entry whose :value is VALUE, or nil."
  (cl-find value (godel-models) :key (lambda (m) (plist-get m :value)) :test #'equal))

(defun godel-models-router-p (entry)
  "Non-nil if ENTRY is the Not Diamond router (identified by :resolved)."
  (string-prefix-p "notdiamond" (or (plist-get entry :resolved) "")))

(defun godel-models-parse-initialize (output)
  "Parse the model list out of OUTPUT from an `initialize' request.
OUTPUT is the program's stdout; only lines starting with \"{\" are JSON
\(the wrapper prints plain-text lines first).  Returns a list of model
plists, or nil if no `control_response' with models was found."
  (catch 'found
    (dolist (line (split-string output "\n" t))
      (when (string-prefix-p "{" line)
        (let* ((msg (ignore-errors (json-parse-string line :object-type 'alist
                                                      :array-type 'list)))
               (models (and (equal (alist-get 'type msg) "control_response")
                            (alist-get 'models (alist-get 'response
                                                          (alist-get 'response msg))))))
          (when models
            (throw 'found
                   (mapcar (lambda (m)
                             (list :value (alist-get 'value m)
                                   :name (alist-get 'displayName m)
                                   :description (alist-get 'description m)
                                   :efforts (alist-get 'supportedEffortLevels m)
                                   :resolved (or (alist-get 'resolvedModel m)
                                                 (alist-get 'value m))))
                           models))))))
    nil))

(defvar godel-models--refreshing nil)

(defun godel-models--run (program args callback &optional input env)
  "Run PROGRAM with ARGS asynchronously; call CALLBACK with (EXIT OUTPUT).
INPUT, if non-nil, is written to stdin, which is then closed.  ENV, if
non-nil, replaces `process-environment' for the child."
  (let* ((buf (generate-new-buffer " *godel-run*"))
         (process-environment (or env process-environment))
         (proc (make-process
                :name "godel-run" :buffer buf :noquery t
                :command (cons program args)
                :connection-type 'pipe
                :stderr (get-buffer-create " *godel-run-stderr*")
                :sentinel (lambda (p _event)
                            (unless (process-live-p p)
                              (let ((out (with-current-buffer (process-buffer p)
                                           (buffer-string))))
                                (kill-buffer (process-buffer p))
                                (funcall callback (process-exit-status p) out)))))))
    (when input
      (process-send-string proc input)
      (process-send-eof proc))
    proc))

(defun godel-models--versions (callback)
  "Call CALLBACK with a string identifying the Claude and notdiamond versions."
  (godel-models--run
   godel-claude-binary '("--version")
   (lambda (_ claude)
     (godel-models--run
      godel-notdiamond-program '("--version")
      (lambda (_ nd)
        (funcall callback (format "claude %s / notdiamond %s"
                                  (string-trim claude) (string-trim nd))))))))

(defun godel-models-refresh (&optional callback)
  "Ask Claude for its model list in the background and cache it.
CALLBACK, if non-nil, is called with the new list (or nil on failure)."
  (interactive)
  (unless godel-models--refreshing
    (setq godel-models--refreshing t)
    (godel-models--versions
     (lambda (versions)
       (godel-models--run
        godel-claude-program
        '("-p" "--input-format" "stream-json" "--output-format" "stream-json" "--verbose")
        (lambda (_exit output)
          (setq godel-models--refreshing nil)
          (let ((models (godel-models-parse-initialize output)))
            (if (not models)
                (message "godel: couldn't read the model list; keeping %s"
                         (if (plist-get godel-models--cache :models) "the cached one" "the default"))
              (setq godel-models--cache (list :models models :versions versions
                                              :fetched (float-time)))
              (godel-task--write-file (godel-models-cache-file) godel-models--cache)
              (when (called-interactively-p 'any)
                (message "godel: %d models" (length models))))
            (godel-models-check-pin)
            (when callback (funcall callback models))))
        (concat (json-encode '((type . "control_request") (request_id . "godel-1")
                               (request . ((subtype . "initialize")))))
                "\n")
        (godel-models--clean-env))))))

(defun godel-models-refresh-if-stale ()
  "Refresh the model list in the background if Claude or notdiamond changed."
  (godel-models--load-cache)
  (godel-models--versions
   (lambda (versions)
     (unless (equal versions (plist-get godel-models--cache :versions))
       (godel-models-refresh)))))

(defalias 'godel-refresh-model-options #'godel-models-refresh)

;;;; Launch environment

(defun godel-models--clean-env (&optional env)
  "Return ENV (default `process-environment') with the pin variables unset.
The entries are removed, and bare names are put in front, which unsets
them for child processes even if something later appends a value."
  (append godel-pin-variables
          (cl-remove-if (lambda (entry)
                          (cl-some (lambda (var) (string-prefix-p (concat var "=") entry))
                                   godel-pin-variables))
                        (or env process-environment))))

(defun godel-models-launch-env (entry effort &optional env)
  "Return the launch settings for model ENTRY with EFFORT.
ENV defaults to `process-environment'.  Returns a plist:
  :env       environment for the launch (bind `process-environment' to it)
  :args      extra CLI arguments
  :expect    model `SessionStart' must report
  :pinned    non-nil for a pinned launch
Not Diamond (the router) never gets the pin variables and no --effort."
  (let ((clean (godel-models--clean-env env)))
    (if (godel-models-router-p entry)
        (list :env clean :args nil :expect (plist-get entry :resolved) :pinned nil)
      (list :env (append (list (concat "NOTDIAMOND_RESEARCH_MODEL_PIN=" (plist-get entry :value))
                               (concat "NOTDIAMOND_RESEARCH_EFFORT_PIN=" effort)
                               "NOTDIAMOND_ROUTING=off")
                         ;; Drop the bare names `godel-models--clean-env' added.
                         (nthcdr (length godel-pin-variables) clean))
            :args (list "--effort" effort)
            :expect (plist-get entry :resolved)
            :pinned t))))

(defun godel-models-inherited-pins ()
  "Return the pin variables set in Emacs's own environment."
  (cl-remove-if-not #'getenv godel-pin-variables))

;;;; Pin check

(defconst godel-models--dir (file-name-directory (or load-file-name buffer-file-name)))

(defcustom godel-probe-program (expand-file-name "bin/godel-probe" godel-models--dir)
  "Stand-in Claude binary used by `godel-models-check-pin'."
  :group 'godel
  :type 'file)

(defvar godel-models-pin-status nil
  "Result of the last pin check: (STATUS . DETAIL).
STATUS is `ok', `broken' or `unknown' (the check couldn't run).")

(defun godel-models--notdiamond-config ()
  (expand-file-name (or (getenv "NOTDIAMOND_CONFIG") "~/.notdiamond/config.json")))

(defun godel-models--probe (dir env callback)
  "Run the wrapper against `godel-probe-program' with ENV, in temp DIR.
Calls CALLBACK with the argument list the wrapper passed, or nil."
  (let ((config (make-hash-table :test #'equal))
        (copy (expand-file-name "config.json" dir))
        (out (expand-file-name "args" dir)))
    (when (file-readable-p (godel-models--notdiamond-config))
      (setq config (with-temp-buffer
                     (insert-file-contents (godel-models--notdiamond-config))
                     (json-parse-buffer))))
    (puthash "claudeBinaryPath" godel-probe-program config)
    (with-temp-file copy (insert (json-serialize config)))
    (when (file-exists-p out) (delete-file out))
    (let ((proc (godel-models--run
                 godel-claude-program nil
                 (lambda (_exit _output)
                   (funcall callback
                            (when (file-exists-p out)
                              (with-temp-buffer
                                (set-buffer-multibyte nil)
                                (insert-file-contents-literally out)
                                (split-string (decode-coding-string (buffer-string) 'utf-8)
                                              "\0" t)))))
                 "" (append (list (concat "NOTDIAMOND_CONFIG=" copy)
                                  (concat "GODEL_PROBE_OUT=" out)
                                  (concat "GODEL_PROBE_REAL=" godel-claude-binary))
                            env))))
      (run-with-timer 30 nil (lambda () (when (process-live-p proc) (kill-process proc)))))))

(defun godel-models--check-result (pinned-args router-args pin)
  "Judge the pin check from the arguments the wrapper passed.
PINNED-ARGS is from a launch pinned to PIN, ROUTER-ARGS from a Not
Diamond launch.  Returns (STATUS . DETAIL)."
  (let ((pinned (cadr (member "--model" pinned-args)))
        (router (cadr (member "--model" router-args))))
    (cond
     ((or (null pinned-args) (null router-args))
      (cons 'unknown "the wrapper didn't start the stand-in Claude binary"))
     ((not (equal pinned pin))
      (cons 'broken (format "a launch pinned to %s got --model %s" pin (or pinned "nothing"))))
     ((not (string-prefix-p "notdiamond" (or router "")))
      (cons 'broken (format "a Not Diamond launch got --model %s" (or router "nothing"))))
     (t (cons 'ok (format "pinned launch got --model %s, Not Diamond got --model %s"
                          pinned router))))))

(defun godel-models-check-pin (&optional callback)
  "Check that the notdiamond wrapper still honours the model pin.
Runs the wrapper twice against a stand-in Claude binary (no session, no
model call): once pinned, once for Not Diamond.  Sets
`godel-models-pin-status', warns if the pin is broken, and calls
CALLBACK with the status."
  (interactive)
  (let* ((interactive (called-interactively-p 'any))
         (entry (or (cl-find-if-not #'godel-models-router-p (godel-models))
                    (cl-find-if-not #'godel-models-router-p godel-model-options)))
         (router (or (cl-find-if #'godel-models-router-p (godel-models))
                     (cl-find-if #'godel-models-router-p godel-model-options)))
         (dir (make-temp-file "godel-pin-check" t))
         (finish (lambda (status)
                   (delete-directory dir t)
                   (setq godel-models-pin-status status)
                   (when godel-models--cache
                     (setq godel-models--cache (plist-put godel-models--cache :pin-status status))
                     (godel-task--write-file (godel-models-cache-file) godel-models--cache))
                   (pcase (car status)
                     ('broken (display-warning
                               'godel (format "The notdiamond wrapper no longer honours godel's model pin (%s).  Pinned models are unavailable; Not Diamond still works."
                                              (cdr status))))
                     ('unknown (message "godel: couldn't check the model pin: %s" (cdr status)))
                     ('ok (when interactive
                            (message "godel: model pin works (%s)" (cdr status)))))
                   (when callback (funcall callback status)))))
    ;; The pinned run sets NOTDIAMOND_ROUTING=off; it must never be the one
    ;; that starts the proxy.
    (godel-models-ensure-proxy
     (lambda (ok &optional err)
       (if (not ok)
           (funcall finish (cons 'unknown (or err "proxy check failed")))
         (godel-models--probe
          dir (plist-get (godel-models-launch-env entry "low") :env)
          (lambda (pinned-args)
            (godel-models--probe
             dir (plist-get (godel-models-launch-env router "low") :env)
             (lambda (router-args)
               (funcall finish (godel-models--check-result
                                pinned-args router-args (plist-get entry :value))))))))))))

(defun godel-models-pin-broken-p ()
  "Non-nil if the last pin check found the pin not working."
  (eq (car godel-models-pin-status) 'broken))

;;;; Proxy guard

(defcustom godel-proxy-start-timeout 20
  "Seconds to wait for a proxy started by godel to report ok."
  :group 'godel
  :type 'integer)

(defun godel-models--proxy-ok-p (output)
  (let ((status (ignore-errors
                  (json-parse-string (string-trim output) :object-type 'alist))))
    (eq (alist-get 'ok status) t)))

(defun godel-models-ensure-proxy (callback)
  "Make sure the notdiamond proxy is running, then call CALLBACK with t.
If no proxy is running, start one with the pin variables unset, so it
can't inherit routing off from a pinned launch.  CALLBACK gets nil and
an error message if the proxy doesn't come up."
  (godel-models--run
   godel-notdiamond-program '("status" "--json")
   (lambda (_ out)
     (if (godel-models--proxy-ok-p out)
         (funcall callback t)
       (message "godel: starting the notdiamond proxy")
       (godel-models--run
        godel-notdiamond-program '("start")
        (lambda (_ _)
          (godel-models--wait-proxy (+ (float-time) godel-proxy-start-timeout) callback))
        nil (godel-models--clean-env))))))

(defun godel-models--wait-proxy (deadline callback)
  (godel-models--run
   godel-notdiamond-program '("status" "--json")
   (lambda (_ out)
     (cond ((godel-models--proxy-ok-p out) (funcall callback t))
           ((> (float-time) deadline)
            (funcall callback nil "the notdiamond proxy didn't start"))
           (t (run-with-timer 0.5 nil #'godel-models--wait-proxy deadline callback))))))

(provide 'godel-models)
;;; godel-models.el ends here
