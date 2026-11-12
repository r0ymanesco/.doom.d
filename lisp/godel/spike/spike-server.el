;;; spike-server.el --- event sink for the godel phase 0 spike -*- lexical-binding: t -*-
(require (quote json))
(defvar godel-spike-events nil "Received hook events, newest first.")
(defun godel-status-event (event payload)
  (push (list (float-time) event
              (json-parse-string
               (decode-coding-string (base64-decode-string payload) 'utf-8)
               :object-type 'alist))
        godel-spike-events)
  nil)
(defun godel-spike-dump ()
  "Return events oldest first as JSON."
  (json-encode
   (mapcar (lambda (e) `((time . ,(nth 0 e)) (event . ,(nth 1 e)) (data . ,(nth 2 e))))
           (reverse godel-spike-events))))
