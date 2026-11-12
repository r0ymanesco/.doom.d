;;; godel-tui.el --- Reading the Claude Code TUI -*- lexical-binding: t -*-

;;; Commentary:
;; The few places godel has to look at Claude's screen: waiting for the input
;; box before sending a prompt, and the workspace trust dialog in repos Claude
;; hasn't seen.  Everything else comes from hooks.  These checks depend on
;; screen text, so they look for the exact thing they need and give up after a
;; timeout rather than guessing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar ghostel--term)
(defvar ghostel--term-rows)
(declare-function ghostel--copy-all-text "ghostel-module" (term))

(defun godel-tui--tail (text n)
  "The last N lines of TEXT."
  (let ((lines (split-string text "\n")))
    (string-join (last lines n) "\n")))

(defun godel-tui-screen (buffer)
  "Return the bottom screenful of BUFFER's terminal as plain text.
ghostel doesn't render into a buffer that isn't displayed, so the text is
read from the terminal emulator itself (`ghostel--copy-all-text').  That
returns the whole scrollback; only the last screen's worth of lines is
kept, so text that has scrolled away (e.g. an answered dialog) can't
match.  Returns nil if BUFFER is gone."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (and (bound-and-true-p ghostel--term) (fboundp 'ghostel--copy-all-text))
          (godel-tui--tail (or (ghostel--copy-all-text ghostel--term) "")
                           (max 24 (or (bound-and-true-p ghostel--term-rows) 0)))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defconst godel-tui--sp "[ \t\u00a0]"
  "Regexp for one space on Claude's screen; it uses no-break spaces too.")

(defun godel-tui-trust-dialog-p (text)
  "Non-nil if TEXT shows the workspace trust dialog."
  (and text (string-match-p "Yes, I trust this folder" text)))

(defun godel-tui-trust-cursor-on-yes-p (text)
  "Non-nil if the trust dialog's cursor is on the \"Yes\" option."
  (and text (string-match-p (concat "^" godel-tui--sp "*❯" godel-tui--sp "*"
                                    "\\(?:[0-9]+\\." godel-tui--sp "*\\)?Yes, I trust this folder")
                            text)))

(defun godel-tui-input-ready-p (text)
  "Non-nil if TEXT shows Claude's input box and no dialog or menu."
  (and text
       (not (godel-tui-trust-dialog-p text))
       ;; The input line starts with ❯ at column 0; menu rows are indented
       ;; and numbered.
       (string-match-p (concat "^❯\\(?:" godel-tui--sp "\\|$\\)") text)
       (not (string-match-p (concat "^" godel-tui--sp "*❯" godel-tui--sp "*[0-9]+\\.") text))))

(defun godel-tui-wait (buffer predicate timeout callback)
  "Poll BUFFER until PREDICATE is non-nil for its text, or TIMEOUT seconds.
Calls CALLBACK with the text when PREDICATE matches, or with nil on
timeout or if BUFFER dies."
  (let ((deadline (+ (float-time) timeout)))
    (cl-labels ((poll ()
                  (let ((text (godel-tui-screen buffer)))
                    (cond ((null text) (funcall callback nil))
                          ((funcall predicate text) (funcall callback text))
                          ((> (float-time) deadline) (funcall callback nil))
                          (t (run-with-timer 0.2 nil #'poll))))))
      (poll))))

(provide 'godel-tui)
;;; godel-tui.el ends here
