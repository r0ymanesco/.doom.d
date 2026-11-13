;;; godel-compose.el --- Text entry buffers for godel -*- lexical-binding: t -*-

;;; Commentary:
;; Buffers for writing a task prompt (and, later, comments and feedback).
;; They behave like magit's commit buffer: `godel-compose-keys-mode' remaps
;; the evil commands behind the ex commands, as with-editor does, so
;;   :wq :x ZZ C-c C-c   submit
;;   :q :q! ZQ C-c C-k   cancel (always, even after :w)
;;   :w                  save the draft and keep editing
;; Each buffer visits a draft file under <git-common-dir>/godel/drafts/, so
;; nothing is lost if Emacs crashes.  A cancelled draft is kept and offered
;; again next time.
;;
;; A buffer can open before its text is ready (ship's draft comes from the
;; author): `godel-compose-wait' shows a status in place of the text and
;; makes the buffer read-only, `godel-compose-fill' puts the text in.

;;; Code:

(require 'subr-x)
(require 'godel-task)
(require 'markdown-mode)

(declare-function evil-insert-state "evil-states" (&optional arg))

(defvar-local godel-compose--callback nil
  "Function called with the buffer's text on submit.")
(defvar-local godel-compose--kind nil)
(defvar-local godel-compose--repo nil)
(defvar-local godel-compose--waiting nil
  "Overlay showing what the buffer is waiting for, while its text isn't ready.")

(defconst godel-compose-restorable-kinds '(task)
  "Kinds whose cancelled drafts are offered again next time.")

(defun godel-compose--text ()
  "The buffer's text (a quote, if any, is only displayed above it)."
  (string-trim (buffer-substring-no-properties (point-min) (point-max))))

(defvar godel-compose--last-cancelled nil
  "Alist of (KIND . DRAFT-FILE) for the last cancelled draft of each kind.")

(define-derived-mode godel-compose-mode markdown-mode "godel-compose"
  "Major mode for godel text entry buffers.")

(defun godel-compose-submit ()
  "Submit this buffer's text."
  (interactive)
  (when godel-compose--waiting
    (user-error "godel: not ready yet; :q! to cancel"))
  (let ((text (godel-compose--text))
        (callback godel-compose--callback)
        (file buffer-file-name))
    (if (string-empty-p text)
        (progn (message "godel: empty, cancelled") (godel-compose--close nil))
      (godel-compose--close nil)
      (when (and file (file-exists-p file)) (delete-file file))
      (funcall callback text))))

(defun godel-compose-cancel ()
  "Cancel; keep the draft so it can be restored next time."
  (interactive)
  (let ((text (godel-compose--text)))
    (if (string-empty-p text)
        (godel-compose--close nil)
      (let ((save-silently t)) (save-buffer))
      (setf (alist-get godel-compose--kind godel-compose--last-cancelled) buffer-file-name)
      (godel-compose--close nil)
      (message "godel: cancelled; the draft will be offered next time"))))

(defun godel-compose--close (_)
  (let ((buffer (current-buffer))
        (window (get-buffer-window (current-buffer))))
    (set-buffer-modified-p nil)
    (when (and window (not (one-window-p t window)))
      (quit-window nil window))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defvar-keymap godel-compose-keys-mode-map
  "C-c C-c" #'godel-compose-submit
  "<remap> <evil-save-and-close>" #'godel-compose-submit
  "<remap> <evil-save-modified-and-close>" #'godel-compose-submit
  "C-c C-k" #'godel-compose-cancel
  "<remap> <kill-buffer>" #'godel-compose-cancel
  "<remap> <evil-quit>" #'godel-compose-cancel)

(define-minor-mode godel-compose-keys-mode
  "Submit and cancel keys for godel compose buffers."
  :keymap godel-compose-keys-mode-map)

(defun godel-compose--draft-file (repo kind)
  (expand-file-name (format "%s-%s.md" kind (format-time-string "%Y%m%d-%H%M%S"))
                    (expand-file-name "drafts" (godel-task-store-dir repo))))

(defun godel-compose--header (header)
  (concat " " header "   "
          (substitute-command-keys
           "\\<godel-compose-keys-mode-map>:wq or \\[godel-compose-submit] submit · :q! or \\[godel-compose-cancel] cancel")))

(defface godel-compose-waiting '((t :inherit warning :height 1.2))
  "Status shown in a compose buffer whose text isn't ready yet."
  :group 'godel)

(defun godel-compose-wait (buffer status)
  "Show STATUS in BUFFER in place of its text, which isn't ready yet.
The buffer is read-only and can't be submitted until `godel-compose-fill';
call again to update STATUS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless godel-compose--waiting
        (setq godel-compose--waiting (make-overlay (point-min) (point-min) nil nil nil))
        (setq buffer-read-only t)
        (when (fboundp 'evil-normal-state)
          (dolist (w (get-buffer-window-list buffer nil t))
            (with-selected-window w (evil-normal-state)))))
      (overlay-put godel-compose--waiting 'before-string
                   (concat "\n  " (propertize status 'face 'godel-compose-waiting) "\n")))))

(defun godel-compose-fill (buffer text &optional header)
  "End BUFFER's wait: insert TEXT, make it editable, set HEADER if given."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when godel-compose--waiting
        (delete-overlay godel-compose--waiting)
        (setq godel-compose--waiting nil))
      (setq buffer-read-only nil)
      (when (zerop (buffer-size)) (insert text))
      (when header (setq header-line-format (godel-compose--header header)))
      (set-buffer-modified-p nil)
      (dolist (w (get-buffer-window-list buffer nil t))
        (set-window-point w (point-min))
        (when (and (eq w (selected-window)) (fboundp 'evil-insert-state))
          (with-selected-window w (goto-char (point-min)) (evil-insert-state)))))))

(defun godel-compose (repo kind header callback &optional initial quote)
  "Open a compose buffer for KIND (a symbol) in REPO.
HEADER is shown in the header line.  CALLBACK gets the text on submit.
INITIAL is inserted.  QUOTE, if non-nil, is displayed above the text
\(an overlay, so it can't be edited and isn't saved or submitted).  For kinds in
`godel-compose-restorable-kinds', a cancelled draft is offered first."
  (let* ((last (and (memq kind godel-compose-restorable-kinds)
                    (alist-get kind godel-compose--last-cancelled)))
         (restore (and last (file-exists-p last)
                       (y-or-n-p "godel: restore the draft you cancelled last time? ")))
         (file (if restore last (godel-compose--draft-file repo kind))))
    (setf (alist-get kind godel-compose--last-cancelled nil 'remove) nil)
    (make-directory (file-name-directory file) t)
    ;; No file-type detection: it would run markdown-mode and the user's
    ;; `markdown-mode-hook' before godel-compose-mode takes over.
    (let ((buffer (let ((auto-mode-alist nil) (magic-mode-alist nil)
                        (magic-fallback-mode-alist nil))
                    (find-file-noselect file))))
      (with-current-buffer buffer
        ;; Not the user's markdown-mode-hook: settings like inline images or
        ;; markup hiding don't suit a prompt draft (and images fail in a
        ;; terminal frame).
        (let ((markdown-mode-hook nil)) (godel-compose-mode))
        (godel-compose-keys-mode 1)
        (setq godel-compose--callback callback
              godel-compose--kind kind
              godel-compose--repo repo
              header-line-format (godel-compose--header header))
        (when (and initial (not restore) (zerop (buffer-size)))
          (insert initial))
        (when quote
          (let ((ov (make-overlay (point-min) (point-min) nil nil nil)))
            (overlay-put ov 'before-string
                         (propertize (concat quote "\n") 'face 'shadow))
            (overlay-put ov 'godel-compose-quote t))))
      (pop-to-buffer buffer)
      ;; Start below any read-only quote.
      (goto-char (point-max))
      (when (fboundp 'evil-insert-state) (evil-insert-state))
      buffer)))

(provide 'godel-compose)
;;; godel-compose.el ends here
