;;; godel-api.el --- Entry points for frontends outside Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; Small, stable functions that other frontends (a Slack bot, a web page, a
;; script) can call through emacsclient.  Everything takes a task ID and
;; returns plain data.  See PLAN.md, "Core rules".

;;; Code:

(require 'godel-task)
(require 'godel-agent)

(defun godel-api-tasks ()
  "Return all tasks as a list of plists."
  (godel-task-load-all)
  (godel-task-all))

(defun godel-api-task (id)
  "Return task ID as a plist."
  (godel-task-get id))

(defun godel-api-send (id text)
  "Send TEXT to task ID's agent."
  (godel-agent-send (or (godel-task-get id) (error "godel: no task %s" id)) text)
  t)

(defun godel-api-stop (id)
  "Stop task ID's session."
  (godel-agent-stop (or (godel-task-get id) (error "godel: no task %s" id)))
  t)

(provide 'godel-api)
;;; godel-api.el ends here
