;;; godel-review.el --- Task diffs and review comments -*- lexical-binding: t -*-

;;; Commentary:
;; Reviewing a task's work (PLAN.md, "Review" and the Phase 2 decisions).
;;
;; Diff: `godel-review-diff' shows the task's working tree, including
;; uncommitted and untracked changes, against the merge base with its base
;; branch, in a magit diff buffer.  Untracked files are made visible by
;; pointing git at a temporary copy of the index in which they're added as
;; intent-to-add (through a buffer-local `magit-git-environment'); the
;; worktree's real index is never touched.
;;
;; Comments: `godel-comment-mode' shows comments under their lines, in task
;; diffs and in files inside a task's worktree.  In task diffs,
;; `godel-diff-comment-mode' adds keys: `C' comments on the line or region
;; at point (or edits the comment there), `gn' / `gp' move between
;; comments.  In files, `godel-review-comment' is on SPC G c (evil's `C'
;; stays "change to end of line").
;; Comments are stored on the task as plists:
;;   (:id N :file "rel/path" :start L :end L :anchor "the lines' text"
;;    :text "..." :state pending|sent|addressed|outdated :created TIME)
;; When the agent changes a file, a comment is found again by its anchor
;; text; if the text is gone it becomes outdated.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'godel-task)
(require 'godel-compose)

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-make-intercept-map "evil-core" (keymap &optional state aux))
(declare-function evil-normalize-keymaps "evil-core" (&optional state))

(defface godel-comment '((t :inherit font-lock-doc-face :box (:line-width -1)))
  "Review comments shown in diffs and files."
  :group 'godel)

(defface godel-comment-dim '((t :inherit (shadow godel-comment)))
  "Sent, addressed or outdated review comments."
  :group 'godel)

;;;; Git helpers

(defun godel-review-merge-base (task)
  "The merge base of TASK's HEAD with its base branch."
  (godel-task--git (plist-get task :worktree) "merge-base" (plist-get task :base) "HEAD"))

(defun godel-review--temp-index (task)
  "Path of TASK's temporary index (under `godel-directory')."
  (expand-file-name (format "tmp/index-%s" (plist-get task :id)) godel-directory))

(defun godel-review--rebuild-index (task)
  "Copy TASK's index and add its untracked files to the copy as intent-to-add."
  (let* ((default-directory (plist-get task :worktree))
         (real (godel-task--git default-directory "rev-parse" "--path-format=absolute"
                                "--git-path" "index"))
         (temp (godel-review--temp-index task)))
    (make-directory (file-name-directory temp) t)
    (if (and real (file-exists-p real))
        (copy-file real temp t)
      (when (file-exists-p temp) (delete-file temp)))
    (let ((process-environment (cons (concat "GIT_INDEX_FILE=" temp) process-environment)))
      (process-file "git" nil nil nil "add" "--intent-to-add" "--all" "."))
    temp))

;;;; Diff

(defvar godel-review--diff-task nil
  "Task ID while `godel-review-diff' sets up a diff buffer.")

(defvar-local godel-review-task-id nil
  "In a task diff, the ID of its task.")

(defun godel-review--setup-diff-buffer ()
  "In `magit-diff-mode-hook': prepare a task diff before its first refresh."
  (when godel-review--diff-task
    (let ((task (godel-task-get godel-review--diff-task)))
      (setq godel-review-task-id godel-review--diff-task)
      (setq-local magit-git-environment
                  (cons (concat "GIT_INDEX_FILE=" (godel-review--temp-index task))
                        (default-value 'magit-git-environment)))
      (godel-comment-mode 1)
      (godel-diff-comment-mode 1))))

(add-hook 'magit-diff-mode-hook #'godel-review--setup-diff-buffer)

(defun godel-review--before-refresh (&rest _)
  "Before a task diff refreshes, pick up new untracked files."
  (when-let* ((task (and godel-review-task-id (godel-task-get godel-review-task-id))))
    (godel-review--rebuild-index task)))

(advice-add 'magit-diff-refresh-buffer :before #'godel-review--before-refresh)

(defun godel-review-diff (task)
  "Show TASK's changes against its base branch, with comments."
  (let ((default-directory (plist-get task :worktree))
        (base (or (godel-review-merge-base task)
                  (user-error "godel: no merge base with %s" (plist-get task :base)))))
    (godel-review--rebuild-index task)
    (let ((godel-review--diff-task (plist-get task :id)))
      ;; A diff buffer is reused per repository; make sure ours is fresh.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'magit-diff-mode)
                     (equal (plist-get task :id) godel-review-task-id))
            (kill-buffer buffer))))
      (magit-diff-setup-buffer base nil (list "--no-ext-diff") nil))))

;;;; Comments: data

(defun godel-review-comments (task &optional states)
  "TASK's comments, optionally only those in STATES."
  (cl-remove-if-not (lambda (c) (or (null states) (memq (plist-get c :state) states)))
                    (plist-get task :comments)))

(defun godel-review--set-comments (id comments)
  (godel-task-update id :comments comments))

(defun godel-review-add-comment (id file start end text)
  "Add a pending comment on FILE lines START..END to task ID."
  (let* ((task (godel-task-get id))
         (seq (1+ (or (plist-get task :comment-seq) 0)))
         (comment (list :id seq :file file :start start :end end
                        :anchor (godel-review--file-lines task file start end)
                        :text text :state 'pending :created (float-time))))
    (godel-task-update id :comment-seq seq
                       :comments (append (plist-get task :comments) (list comment)))
    comment))

(defun godel-review-update-comment (id comment-id &rest props)
  "Set PROPS on comment COMMENT-ID of task ID."
  (godel-review--set-comments
   id (mapcar (lambda (c)
                (if (equal (plist-get c :id) comment-id)
                    (let ((c (copy-sequence c)))
                      (while props (setq c (plist-put c (pop props) (pop props))))
                      c)
                  c))
              (plist-get (godel-task-get id) :comments))))

(defun godel-review-delete-comment (id comment-id)
  "Delete comment COMMENT-ID from task ID."
  (godel-review--set-comments
   id (cl-remove comment-id (plist-get (godel-task-get id) :comments)
                 :key (lambda (c) (plist-get c :id)) :test #'equal)))

(defun godel-review--file-lines (task file start end)
  "Text of FILE's lines START..END in TASK's worktree, or nil."
  (let ((path (expand-file-name file (plist-get task :worktree))))
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (when (zerop (forward-line (1- start)))
          (let ((beg (point)))
            (forward-line (1+ (- end start)))
            (string-trim-right (buffer-substring-no-properties beg (point)) "\n")))))))

(defun godel-review-resolve (task comment)
  "Find COMMENT in its file again.  Returns the comment, updated if needed.
If its lines still hold the anchor text, nothing changes.  Otherwise the
anchor is searched for (nearest match to the old position wins) and the
comment moves there.  If the text is gone, an open comment becomes
`outdated'."
  (let* ((file (plist-get comment :file))
         (start (plist-get comment :start))
         (len (1+ (- (plist-get comment :end) start)))
         (anchor (plist-get comment :anchor))
         (path (expand-file-name file (plist-get task :worktree))))
    (cond
     ((or (null anchor) (memq (plist-get comment :state) '(outdated addressed))) comment)
     ((equal (godel-review--file-lines task file start (+ start len -1)) anchor) comment)
     (t
      (let ((found (and (file-readable-p path)
                        (with-temp-buffer
                          (insert-file-contents path)
                          (let (hits)
                            (goto-char (point-min))
                            (while (search-forward anchor nil t)
                              (let ((line (line-number-at-pos (match-beginning 0))))
                                (when (save-excursion (goto-char (match-beginning 0)) (bolp))
                                  (push line hits)))
                              (goto-char (1+ (match-beginning 0))))
                            (car (sort hits (lambda (a b) (< (abs (- a start)) (abs (- b start)))))))))))
        (if found
            (plist-put (plist-put (copy-sequence comment) :start found) :end (+ found len -1))
          (plist-put (copy-sequence comment) :state 'outdated)))))))

(defun godel-review-resolve-all (id)
  "Re-find all of task ID's comments; save if anything moved."
  (let* ((task (godel-task-get id))
         (old (plist-get task :comments))
         (new (mapcar (lambda (c) (godel-review-resolve task c)) old)))
    (unless (equal old new)
      (godel-review--set-comments id new))
    new))

;;;; Comments: where point is

(defun godel-review--buffer-task ()
  "The task this buffer belongs to (a task diff, or a file in a worktree)."
  (or (and godel-review-task-id (godel-task-get godel-review-task-id))
      (and buffer-file-name (godel-task-by-directory (file-name-directory buffer-file-name)))))

(defun godel-review--diff-line-at (pos)
  "In a magit diff, (FILE . LINE) of the new-file line at POS, or nil."
  (save-excursion
    (goto-char pos)
    (let ((section (magit-current-section)))
      (when (magit-section-match 'hunk section)
        (cons (magit-section-parent-value section)
              (magit-diff-hunk-line section nil))))))

(defun godel-review--target ()
  "(FILE START END) for the line or region at point, file relative to the worktree."
  (let* ((task (or (godel-review--buffer-task) (user-error "godel: not in a task")))
         (region (use-region-p))
         (beg (if region (region-beginning) (point)))
         (end (if region (max beg (1- (region-end))) (point))))
    (if (derived-mode-p 'magit-diff-mode)
        (let ((a (godel-review--diff-line-at beg))
              (b (godel-review--diff-line-at end)))
          (unless (and a b (equal (car a) (car b)))
            (user-error "godel: put point (or the region) on lines of one file's diff"))
          (list (car a) (min (cdr a) (cdr b)) (max (cdr a) (cdr b))))
      (list (file-relative-name buffer-file-name (plist-get task :worktree))
            (line-number-at-pos beg) (line-number-at-pos end)))))

(defun godel-review--comment-at (task file start end)
  "An open comment of TASK overlapping FILE lines START..END."
  (cl-find-if (lambda (c) (and (equal (plist-get c :file) file)
                               (memq (plist-get c :state) '(pending sent))
                               (<= (plist-get c :start) end)
                               (>= (plist-get c :end) start)))
              (plist-get task :comments)))

(defun godel-review-comment ()
  "Comment on the line or region at point; on an existing comment, edit it."
  (interactive)
  (pcase-let* ((task (or (godel-review--buffer-task) (user-error "godel: not in a task")))
               (id (plist-get task :id))
               (`(,file ,start ,end) (godel-review--target))
               (existing (godel-review--comment-at task file start end))
               (start (if existing (plist-get existing :start) start))
               (end (if existing (plist-get existing :end) end))
               (quote (or (godel-review--file-lines task file start end) "")))
    (deactivate-mark)
    (godel-compose
     (plist-get task :repo) 'comment
     (format "%s comment on %s:%s" (if existing "Edit" "New") file
             (if (= start end) start (format "%d-%d" start end)))
     (lambda (text)
       (if existing
           (godel-review-update-comment id (plist-get existing :id) :text text)
         (godel-review-add-comment id file start end text))
       (message "godel: comment saved (%d pending)"
                (length (godel-review-comments (godel-task-get id) '(pending)))))
     (and existing (plist-get existing :text))
     (concat (mapconcat (lambda (l) (concat "> " l)) (split-string quote "\n") "\n")
             "\n"))))

;;;; Comments: display

(defvar-local godel-comment--overlays nil)

(defun godel-review--diff-pos (file line)
  "Position at the end of new-file LINE of FILE in this magit diff, or nil.
Nil too before magit's first refresh, when there are no sections yet."
  (catch 'found
    (unless magit-root-section (throw 'found nil))
    (magit-map-sections
     (lambda (section)
       (when (and (magit-section-match 'hunk section)
                  (equal (magit-section-parent-value section) file))
         (let ((to (oref section to-range)))
           (when (and to (>= line (car to)) (< line (+ (car to) (or (cadr to) 1))))
             (save-excursion
               (goto-char (oref section content))
               (let ((n (car to)))
                 (while (< (point) (oref section end))
                   (unless (eq (char-after) ?-)
                     (when (= n line) (throw 'found (line-end-position)))
                     (setq n (1+ n)))
                   (forward-line 1)))))))))
    nil))

(defun godel-review--comment-string (comment)
  (let* ((open (memq (plist-get comment :state) '(pending sent)))
         (face (if (eq (plist-get comment :state) 'pending) 'godel-comment 'godel-comment-dim))
         (label (format " %s · %s " (plist-get comment :state)
                        (let ((s (plist-get comment :start)) (e (plist-get comment :end)))
                          (if (= s e) (format "line %d" s) (format "lines %d-%d" s e))))))
    (concat "\n"
            (mapconcat (lambda (line)
                         (concat "    " (propertize (concat " " line " ") 'face face)))
                       (split-string (plist-get comment :text) "\n")
                       "\n")
            "\n    " (propertize label 'face (if open 'shadow 'godel-comment-dim)))))

(defun godel-comment-refresh ()
  "Redraw this buffer's comment overlays."
  (mapc #'delete-overlay godel-comment--overlays)
  (setq godel-comment--overlays nil)
  (when-let* ((task (godel-review--buffer-task)))
    (let ((comments (godel-review-resolve-all (plist-get task :id)))
          (file (and buffer-file-name
                     (file-relative-name buffer-file-name (plist-get task :worktree)))))
      (dolist (c comments)
        (unless (memq (plist-get c :state) '(outdated))
          (when-let* ((pos (cond ((derived-mode-p 'magit-diff-mode)
                                  (godel-review--diff-pos (plist-get c :file) (plist-get c :end)))
                                 ((equal file (plist-get c :file))
                                  (save-excursion
                                    (goto-char (point-min))
                                    (and (zerop (forward-line (1- (plist-get c :end))))
                                         (line-end-position)))))))
            (let ((ov (make-overlay pos pos nil t nil)))
              (overlay-put ov 'godel-comment (plist-get c :id))
              (overlay-put ov 'after-string (godel-review--comment-string c))
              (push ov godel-comment--overlays))))))
    (setq godel-comment--overlays
          (sort godel-comment--overlays (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(defun godel-comment-next ()
  "Move to the next comment."
  (interactive)
  (if-let* ((ov (cl-find-if (lambda (o) (> (overlay-start o) (line-end-position)))
                            godel-comment--overlays)))
      (progn (goto-char (overlay-start ov)) (beginning-of-line))
    (message "godel: no more comments")))

(defun godel-comment-previous ()
  "Move to the previous comment."
  (interactive)
  (if-let* ((ov (cl-find-if (lambda (o) (< (overlay-start o) (line-beginning-position)))
                            (reverse godel-comment--overlays))))
      (progn (goto-char (overlay-start ov)) (beginning-of-line))
    (message "godel: no earlier comments")))

(define-minor-mode godel-comment-mode
  "Show a task's review comments under their lines (diffs and files)."
  :lighter nil
  (if godel-comment-mode
      (progn
        (add-hook 'magit-refresh-buffer-hook #'godel-comment-refresh nil t)
        (godel-comment-refresh))
    (remove-hook 'magit-refresh-buffer-hook #'godel-comment-refresh t)
    (mapc #'delete-overlay godel-comment--overlays)
    (setq godel-comment--overlays nil)))

(defvar-keymap godel-diff-comment-mode-map
  "C" #'godel-review-comment
  "g n" #'godel-comment-next
  "g p" #'godel-comment-previous)

(define-minor-mode godel-diff-comment-mode
  "Comment keys in a task diff: C comment, gn / gp next / previous comment.
`C' replaces magit's `magit-commit-add-log', which a review diff doesn't need."
  :keymap godel-diff-comment-mode-map
  (when (fboundp 'evil-normalize-keymaps) (evil-normalize-keymaps)))

(with-eval-after-load 'evil
  (evil-define-key* '(normal motion visual) godel-diff-comment-mode-map
    "C" #'godel-review-comment
    "gn" #'godel-comment-next
    "gp" #'godel-comment-previous)
  ;; evil-collection gives magit's keymaps precedence over evil's state maps,
  ;; so these must be intercept maps to win.
  (dolist (state '(normal motion visual))
    (evil-make-intercept-map godel-diff-comment-mode-map state t)))

;; On diff lines, magit's section keymap is a text property, which beats
;; every evil and minor-mode keymap, and it binds `C' to
;; `magit-commit-add-log'.  Make that binding conditional: godel's comment
;; command in task diffs, magit's command everywhere else.
(defun godel-review--section-C (command)
  (if (bound-and-true-p godel-diff-comment-mode) #'godel-review-comment command))

(with-eval-after-load 'magit-diff
  (keymap-set magit-diff-section-map "C"
              `(menu-item "" magit-commit-add-log :filter godel-review--section-C)))

(defun godel-review--maybe-enable-in-file ()
  "In `find-file-hook': turn on comments for files inside a task worktree."
  (when (and buffer-file-name (godel-task-by-directory (file-name-directory buffer-file-name)))
    (godel-comment-mode 1)))

(add-hook 'find-file-hook #'godel-review--maybe-enable-in-file)

(defun godel-review--refresh-buffers (task change)
  "After a task changes, redraw comments in its diff and file buffers."
  (unless (eq change 'deleted)
    (let ((wt (file-name-as-directory (expand-file-name (plist-get task :worktree)))))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and godel-comment-mode
                     (or (equal godel-review-task-id (plist-get task :id))
                         (and buffer-file-name (string-prefix-p wt (expand-file-name buffer-file-name)))))
            (godel-comment-refresh)))))))

(add-hook 'godel-event-functions #'godel-review--refresh-buffers)

(provide 'godel-review)
;;; godel-review.el ends here
