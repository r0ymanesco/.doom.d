;;; godel-models.el --- Model list, launch environment and proxy guard -*- lexical-binding: t -*-

;;; Commentary:
;; - The model list comes from the Agent SDK `initialize' control request,
;;   sent to `godel-claude-program' in stream-json mode.  It is cached in
;;   `godel-models-cache-file' and refreshed when Claude Code or notdiamond
;;   change version.
;; - `godel-models-launch-env' is the one place that decides the environment
;;   and extra flags for a launch.  Every model except Not Diamond (the
;;   router) is chosen with `--model', which the wrapper passes on to Claude
;;   and the proxy sends through unrouted; the router needs no flag.  Both get
;;   the effort as --effort.  No launch carries the wrapper's pin variables
;;   (see `godel-pin-variables').
;; - `godel-models-ensure-proxy' makes sure the notdiamond proxy is running
;;   and was not started with a pin variable in its environment (which would
;;   turn routing off for every session).
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

(defcustom godel-default-effort "medium"
  "Effort for new tasks and reviewers, on any model.
Not Diamond gets it as --effort only, never as a pin variable; for now the
router still picks the effort for each turn itself."
  :group 'godel
  :type 'string)

(defconst godel-effort-levels '("low" "medium" "high" "xhigh" "max")
  "Effort levels offered when a model entry doesn't say which it supports.")

(defun godel-models-efforts (entry)
  "The effort levels model ENTRY supports, or all of them if it doesn't say."
  (or (plist-get entry :efforts) godel-effort-levels))

(defconst godel-pin-variables
  '("NOTDIAMOND_RESEARCH_MODEL_PIN" "NOTDIAMOND_RESEARCH_EFFORT_PIN" "NOTDIAMOND_ROUTING")
  "The wrapper's internal switches for pinning a session to one model.
godel doesn't use them: it chooses the model with --model.  It removes them
(`godel-scrubbed-variables') because they would override that choice.")

(defconst godel-scrubbed-variables
  (append godel-pin-variables '("ANTHROPIC_API_KEY"))
  "Variables godel removes (`godel-models--clean-env') from the environment of
every Claude launch, and of the notdiamond proxy it starts.
The pin variables: with one set, the wrapper drops the --model godel passes
(\"Ignoring --model\") and runs the pinned model instead, and a proxy that
inherits NOTDIAMOND_ROUTING=off stops routing for every session.  And
ANTHROPIC_API_KEY, which a headless (-p) launch uses without asking, even
when it was declined in interactive sessions.  That would bypass the
forwarded subscription login that Not Diamond is set up to use.  The proxy
doesn't read ANTHROPIC_API_KEY (it forwards the client's own credential),
so dropping it there changes nothing; the pin variables are what matter.")

(defun godel-models-cache-file ()
  (expand-file-name "models.eld" godel-directory))

;;;; Model list

(defvar godel-models--cache nil
  "Plist (:models LIST :versions STRING :fetched TIME), or nil.
A cache file written by an older godel may also have :pin-status, which is
ignored.")

(defun godel-models--load-cache ()
  (unless godel-models--cache
    (setq godel-models--cache (godel-task--read-file (godel-models-cache-file))))
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
  "Return ENV (default `process-environment') with `godel-scrubbed-variables' unset.
The entries are removed, and bare names are put in front, which unsets
them for child processes even if something later appends a value."
  (append godel-scrubbed-variables
          (cl-remove-if (lambda (entry)
                          (cl-some (lambda (var) (string-prefix-p (concat var "=") entry))
                                   godel-scrubbed-variables))
                        (or env process-environment))))

(defun godel-models-launch-env (entry effort &optional env)
  "Return the launch settings for model ENTRY with EFFORT.
ENV defaults to `process-environment'.  Returns a plist:
  :env       environment for the launch (bind `process-environment' to it)
  :args      extra CLI arguments
  :expect    model `SessionStart' must report
The environment never has a pin variable.  Any model but Not Diamond is
chosen with --model, which the wrapper hands to Claude and the proxy sends
through as named, without routing.  Not Diamond needs no flag on a fresh
launch (the wrapper starts it on the router; `godel-agent-flags' adds
--model on a resume, where the wrapper adds none).  Both get --effort, if
EFFORT is non-nil.

EFFORT must be a string or nil; anything else signals here, before anything
is started, rather than failing when the process is."
  (cl-check-type effort (or null string))
  (list :env (godel-models--clean-env env)
        :args (append (unless (godel-models-router-p entry)
                        (list "--model" (plist-get entry :value)))
                      (and effort (list "--effort" effort)))
        :expect (plist-get entry :resolved)))

(defun godel-models-inherited-pins ()
  "Return the pin variables set in Emacs's own environment."
  (cl-remove-if-not #'getenv godel-pin-variables))

(defun godel-models--notdiamond-config ()
  (expand-file-name (or (getenv "NOTDIAMOND_CONFIG") "~/.notdiamond/config.json")))

;;;; Proxy guard

(defcustom godel-proxy-start-timeout 20
  "Seconds to wait for a proxy started by godel to report ok."
  :group 'godel
  :type 'integer)

(defcustom godel-proxy-address nil
  "(HOST . PORT) of the notdiamond proxy, or nil to read it from the
notdiamond config (server.host and server.port)."
  :group 'godel
  :type '(choice (const :tag "From the notdiamond config" nil)
                 (cons string integer)))

(defun godel-models--proxy-address ()
  (or godel-proxy-address
      (let* ((config (ignore-errors
                       (with-temp-buffer
                         (insert-file-contents (godel-models--notdiamond-config))
                         (json-parse-buffer :object-type 'alist))))
             (server (alist-get 'server config)))
        (cons (or (alist-get 'host server) "127.0.0.1")
              (or (alist-get 'port server) 8082)))))

(defun godel-models--proxy-answers-p (callback)
  "Ask the proxy for /stats directly; CALLBACK gets non-nil on HTTP 200.
Much faster than `notdiamond status' (no Node start-up).  Any failure,
or no answer within 2 seconds, gives nil."
  (let* ((address (godel-models--proxy-address))
         (done nil) (response "") proc
         (finish (lambda (ok)
                   (unless done
                     (setq done t)
                     (when (process-live-p proc) (delete-process proc))
                     (funcall callback ok)))))
    (condition-case nil
        (progn
          (setq proc (make-network-process
                      :name "godel-proxy-check" :host (car address) :service (cdr address)
                      :nowait t :noquery t :coding 'binary
                      :filter (lambda (_ out)
                                (setq response (concat response out))
                                (when (string-match "\\`HTTP/1\\.[01] \\([0-9]+\\)" response)
                                  (funcall finish (equal (match-string 1 response) "200"))))
                      :sentinel (lambda (p event)
                                  (cond ((string-prefix-p "open" event)
                                         (process-send-string
                                          p (format "GET /stats HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n\r\n"
                                                    (car address))))
                                        ((not (memq (process-status p) '(open connect)))
                                         (funcall finish nil))))))
          (run-with-timer 2 nil finish nil))
      (error (funcall finish nil)))))

(defun godel-models--proxy-ok-p (output)
  (let ((status (ignore-errors
                  (json-parse-string (string-trim output) :object-type 'alist))))
    (eq (alist-get 'ok status) t)))

(defun godel-models--proxy-up-p (callback)
  "CALLBACK gets non-nil if the proxy is up: asked directly first, then
through `notdiamond status'."
  (godel-models--proxy-answers-p
   (lambda (up)
     (if up
         (funcall callback t)
       (godel-models--run godel-notdiamond-program '("status" "--json")
                          (lambda (_ out) (funcall callback (godel-models--proxy-ok-p out))))))))

(defun godel-models-ensure-proxy (callback)
  "Make sure the notdiamond proxy is running, then call CALLBACK with t.
If no proxy is running, start one with `godel-scrubbed-variables' unset:
the pin variables, so it can't inherit NOTDIAMOND_ROUTING=off from Emacs's
environment (it serves every session), and ANTHROPIC_API_KEY, which it
doesn't use.  CALLBACK gets nil and an error message if the proxy doesn't
come up."
  (godel-models--proxy-up-p
   (lambda (up)
     (if up
         (funcall callback t)
       (message "godel: starting the notdiamond proxy")
       (godel-models--run
        godel-notdiamond-program '("start")
        (lambda (_ _)
          (godel-models--wait-proxy (+ (float-time) godel-proxy-start-timeout) callback))
        nil (godel-models--clean-env))))))

(defun godel-models--wait-proxy (deadline callback)
  (godel-models--proxy-up-p
   (lambda (up)
     (cond (up (funcall callback t))
           ((> (float-time) deadline)
            (funcall callback nil "the notdiamond proxy didn't start"))
           (t (run-with-timer 0.5 nil #'godel-models--wait-proxy deadline callback))))))

(provide 'godel-models)
;;; godel-models.el ends here
