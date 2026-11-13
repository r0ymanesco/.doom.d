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
;;
;; Feedback: `godel-review-list' (v l) lists comments by state;
;; `godel-review-send' (v s) turns the pending ones into one message, which
;; you edit and send to the author's session.  Sent comments become
;; addressed when the agent finishes its next turn.  `godel-review-history'
;; (v h) shows past rounds.  `godel-review-menu' (v) has all of it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'godel-task)
(require 'godel-compose)
(require 'godel-agent)
(require 'godel-status)
(require 'godel-tui)
(require 'transient)

(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-make-intercept-map "evil-core" (keymap &optional state aux))
(declare-function evil-normalize-keymaps "evil-core" (&optional state))

(defface godel-comment '((t :inherit default))
  "Text of open (pending) review comments shown in diffs and files."
  :group 'godel)

(defface godel-comment-dim '((t :inherit shadow))
  "Text of sent, addressed or outdated review comments."
  :group 'godel)

(defface godel-comment-meta '((t :inherit shadow :slant italic))
  "The state and lines under a review comment."
  :group 'godel)

(defface godel-comment-bar '((t :weight bold))
  "The bar in front of a review comment; coloured by its state."
  :group 'godel)

(defconst godel-review--state-colours
  '((pending . yellow) (sent . blue) (addressed . green) (outdated . white))
  "Theme ANSI colour of the bar for each comment state.")

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

(defun godel-review--bar-face (state)
  "The bar's face for a comment in STATE: the theme's colour, foreground only."
  (let* ((colour (alist-get state godel-review--state-colours))
         (fg (and colour (not (eq state 'outdated))
                  (face-foreground (intern (format "ansi-color-%s" colour)) nil t))))
    (if fg (list 'godel-comment-bar (list :foreground fg)) '(godel-comment-bar shadow))))

(defun godel-review--comment-string (comment)
  "COMMENT as display text: a coloured bar, its text, then its state and lines.
No boxes or backgrounds: the text uses the normal text colour so it stays
readable on any theme."
  (let* ((state (plist-get comment :state))
         (text-face (if (eq state 'pending) 'godel-comment 'godel-comment-dim))
         (bar (propertize "    ▍ " 'face (godel-review--bar-face state)))
         (s (plist-get comment :start)) (e (plist-get comment :end))
         (meta (format "%s · %s" state (if (= s e) (format "line %d" s) (format "lines %d-%d" s e)))))
    (concat (mapconcat (lambda (line) (concat bar (propertize line 'face text-face)))
                       (split-string (plist-get comment :text) "\n")
                       "\n")
            "\n" bar (propertize meta 'face 'godel-comment-meta))))

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
            ;; Shown as text *before the next line*, not after this one: Emacs
            ;; can't put the cursor inside display text, and text after the
            ;; end of a line belongs to that line, so `j' would get stuck on
            ;; it.  Only a comment on the last line hangs off its end.
            (let* ((text (godel-review--comment-string c))
                   (last (>= pos (point-max)))
                   (ov (if last (make-overlay pos pos nil t nil)
                         (make-overlay (1+ pos) (1+ pos) nil nil nil))))
              (overlay-put ov 'godel-comment (plist-get c :id))
              (overlay-put ov 'godel-comment-line (save-excursion (goto-char pos)
                                                                  (line-beginning-position)))
              (if last
                  (overlay-put ov 'after-string (concat "\n" text))
                (overlay-put ov 'before-string (concat text "\n")))
              (push ov godel-comment--overlays))))))
    (setq godel-comment--overlays
          (sort godel-comment--overlays (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(defun godel-comment--line (ov)
  "Start of the line OV's comment is on."
  (overlay-get ov 'godel-comment-line))

(defun godel-comment-next ()
  "Move to the next comment's line."
  (interactive)
  (if-let* ((ov (cl-find-if (lambda (o) (> (godel-comment--line o) (line-beginning-position)))
                            godel-comment--overlays)))
      (goto-char (godel-comment--line ov))
    (message "godel: no more comments")))

(defun godel-comment-previous ()
  "Move to the previous comment's line."
  (interactive)
  (if-let* ((ov (cl-find-if (lambda (o) (< (godel-comment--line o) (line-beginning-position)))
                            (reverse godel-comment--overlays))))
      (goto-char (godel-comment--line ov))
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

;;;; Sending feedback

(defun godel-review--send-to-author (task text &optional on-sent)
  "Send TEXT to TASK's author session, resuming it first if needed.
ON-SENT, if non-nil, is called once the text has been sent."
  (godel-agent-ensure-session
   task (lambda (buffer)
          (if (not buffer)
              (message "godel: %s has no session to send to; R on the board starts one"
                       (plist-get task :id))
            ;; A just-resumed session needs its input box before typing.
            (godel-tui-wait buffer #'godel-tui-input-ready-p 20
                            (lambda (screen)
                              (if (not screen)
                                  (message "godel: the session isn't ready for input; nothing sent")
                                (godel-agent-send (godel-task-get (plist-get task :id)) text)
                                (when on-sent (funcall on-sent)))))))))

(defun godel-review-feedback-text (task)
  "One message with TASK's pending comments, each with the code it's about."
  (let ((pending (godel-review-comments task '(pending)))
        (n 0))
    (concat
     "Here's my review of your changes. Please address each point:\n\n"
     (mapconcat
      (lambda (c)
        (let* ((s (plist-get c :start)) (e (plist-get c :end))
               (code (or (godel-review--file-lines task (plist-get c :file) s e)
                         (plist-get c :anchor) "")))
          (format "%d. `%s` %s:\n```\n%s\n```\n%s\n"
                  (cl-incf n) (plist-get c :file)
                  (if (= s e) (format "line %d" s) (format "lines %d-%d" s e))
                  code (plist-get c :text))))
      pending "\n")
     "\n")))

(defun godel-review-send (task)
  "Edit TASK's pending comments as one message, then send it to the author."
  (let* ((id (plist-get task :id))
         (pending (godel-review-comments (godel-review--refreshed task) '(pending))))
    (unless pending (user-error "godel: no pending comments on %s" id))
    (godel-compose
     (plist-get task :repo) 'feedback
     (format "Feedback for %s (%d comment%s)" id (length pending) (if (cdr pending) "s" ""))
     (lambda (text)
       (let ((ids (mapcar (lambda (c) (plist-get c :id)) pending)))
         (godel-review--send-to-author
          (godel-task-get id) text
          (lambda ()
            (let* ((task (godel-task-get id))
                   (round (1+ (length (plist-get task :review-rounds)))))
              (godel-review--set-comments
               id (mapcar (lambda (c)
                            (if (memq (plist-get c :id) ids)
                                (plist-put (plist-put (copy-sequence c) :state 'sent) :round round)
                              c))
                          (plist-get task :comments)))
              (godel-task-update id :review-rounds
                                 (append (plist-get (godel-task-get id) :review-rounds)
                                         (list (list :round round :time (float-time) :text text))))
              (message "godel: feedback round %d sent to %s" round id))))))
     (godel-review-feedback-text (godel-review--refreshed task)))))

(defun godel-review--refreshed (task)
  "TASK after re-finding its comments."
  (godel-review-resolve-all (plist-get task :id))
  (godel-task-get (plist-get task :id)))

(defun godel-review--on-stop (id)
  "When the author finishes a turn, its sent comments count as addressed."
  (let ((task (godel-task-get id)))
    (when (godel-review-comments task '(sent))
      (godel-review--set-comments
       id (mapcar (lambda (c)
                    (if (eq (plist-get c :state) 'sent)
                        (plist-put (copy-sequence c) :state 'addressed)
                      c))
                  (plist-get task :comments))))))

(add-hook 'godel-status-stop-functions #'godel-review--on-stop)

;;;; The review list

(defvar-local godel-review--list-task nil "Task ID shown in this review list.")

(defconst godel-review--list-groups
  '(("Pending" pending) ("Sent" sent) ("Addressed" addressed) ("Outdated" outdated)))

(defun godel-review--list-buffer-name (id) (format "*godel-review: %s*" id))

(defvar-keymap godel-review-mode-map
  :parent magit-section-mode-map
  "RET" #'godel-review-list-visit
  "C" #'godel-review-list-edit
  "x" #'godel-review-list-delete
  "a" #'godel-review-list-accept
  "s" #'godel-review-list-send
  "g r" #'godel-review-list-refresh
  "q" #'quit-window)

(define-derived-mode godel-review-mode magit-section-mode "godel-review"
  "A task's review: findings (a accept, x drop) and comments (RET visit,
C edit, x delete, s send)."
  (setq truncate-lines t)
  (when (boundp '+indent-guides-inhibit-functions)
    (add-hook '+indent-guides-inhibit-functions
              (lambda () (derived-mode-p 'godel-review-mode)))))

(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) godel-review-mode-map
    (kbd "RET") #'godel-review-list-visit
    "C" #'godel-review-list-edit
    "x" #'godel-review-list-delete
    "a" #'godel-review-list-accept
    "s" #'godel-review-list-send
    "gr" #'godel-review-list-refresh
    "q" #'quit-window))

(defun godel-review--insert-comment (task c)
  (let* ((s (plist-get c :start)) (e (plist-get c :end))
         (where (format "%s:%s" (plist-get c :file) (if (= s e) s (format "%d-%d" s e)))))
    (magit-insert-section (godel-comment (plist-get c :id))
      (magit-insert-heading
        (concat "  " (propertize where 'face 'magit-filename 'font-lock-face 'magit-filename)
                "  " (car (split-string (plist-get c :text) "\n"))))
      (dolist (line (split-string (or (godel-review--file-lines task (plist-get c :file) s e)
                                      (plist-get c :anchor) "")
                                  "\n"))
        (insert (propertize (concat "      | " line "\n") 'face 'shadow 'font-lock-face 'shadow)))
      (dolist (line (split-string (plist-get c :text) "\n"))
        (insert "      " line "\n")))))

(declare-function godel-reviewer-accept "godel-reviewer" (id finding-id))
(declare-function godel-reviewer-drop "godel-reviewer" (id finding-id))
(declare-function godel-reviewer-run "godel-reviewer" (task &optional model))
(declare-function godel-reviewer-walkthrough "godel-reviewer" (task &optional model))
(declare-function godel-reviewer--read-model "godel-reviewer" ())
(declare-function godel-ship "godel-ship" (task))

(defun godel-review--insert-finding (task f)
  (let* ((s (plist-get f :start)) (e (plist-get f :end))
         (where (format "%s:%s" (plist-get f :file) (if (= s e) s (format "%d-%d" s e)))))
    (magit-insert-section (godel-finding (plist-get f :id))
      (magit-insert-heading
        (concat "  " (propertize (format "%-6s" (plist-get f :severity))
                                 'font-lock-face (pcase (plist-get f :severity)
                                                   ("high" 'error) ("medium" 'warning) (_ 'shadow)))
                " " (propertize where 'font-lock-face 'magit-filename)
                "  " (car (split-string (or (plist-get f :text) "") "\n"))))
      (dolist (line (split-string (or (godel-review--file-lines task (plist-get f :file) s e) "") "\n"))
        (insert (propertize (concat "      | " line "\n") 'font-lock-face 'shadow)))
      (dolist (line (split-string (or (plist-get f :text) "") "\n"))
        (insert "      " line "\n")))))

(defun godel-review-list-refresh ()
  "Redraw this review list."
  (interactive)
  (let* ((task (or (godel-task-get godel-review--list-task) (user-error "godel: task is gone")))
         (inhibit-read-only t)
         (line (line-number-at-pos)))
    (godel-review-resolve-all (plist-get task :id))
    (setq task (godel-task-get (plist-get task :id)))
    (erase-buffer)
    (magit-insert-section (godel-review-root)
      (insert (propertize (format "Review of %s" (plist-get task :title))
                          'font-lock-face 'magit-section-heading)
              (propertize "    RET visit · C edit · x delete/drop · a accept · s send pending\n\n"
                          'font-lock-face 'shadow))
      (when-let* ((note (plist-get task :review-note)))
        (insert (propertize (format "  Independent review: %s\n\n" note)
                            'font-lock-face (if (eq (plist-get task :review-status) 'failed)
                                                'error 'shadow))))
      (when-let* ((proposed (cl-remove-if-not (lambda (f) (eq (plist-get f :state) 'proposed))
                                              (plist-get task :findings))))
        (magit-insert-section (godel-finding-group 'proposed)
          (magit-insert-heading
            (format "Reviewer findings (%d)    a accept as a comment · x drop" (length proposed)))
          (dolist (f proposed) (godel-review--insert-finding task f))
          (insert "\n")))
      (unless (or (plist-get task :comments) (plist-get task :findings))
        (insert "  No comments yet.  C in the task's diff (d) adds one.\n"))
      (pcase-dolist (`(,label ,state) godel-review--list-groups)
        (when-let* ((cs (godel-review-comments task (list state))))
          (magit-insert-section (godel-comment-group state (memq state '(addressed outdated)))
            (magit-insert-heading (format "%s (%d)" label (length cs)))
            (dolist (c cs) (godel-review--insert-comment task c))
            (insert "\n")))))
    (magit-map-sections (lambda (s) (when (oref s hidden) (magit-section-hide s))))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun godel-review-list (task)
  "Show TASK's review comments."
  (let ((buffer (get-buffer-create (godel-review--list-buffer-name (plist-get task :id)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'godel-review-mode) (godel-review-mode))
      (setq godel-review--list-task (plist-get task :id)
            default-directory (plist-get task :worktree))
      (godel-review-list-refresh))
    (pop-to-buffer buffer)))

(defun godel-review--list-comment ()
  (let ((cid (or (magit-section-value-if 'godel-comment) (user-error "godel: no comment here"))))
    (cl-find cid (plist-get (godel-task-get godel-review--list-task) :comments)
             :key (lambda (c) (plist-get c :id)))))

(defun godel-review-list-visit ()
  "Open the commented file at the comment's line."
  (interactive)
  (let ((c (godel-review--list-comment))
        (task (godel-task-get godel-review--list-task)))
    (find-file-other-window (expand-file-name (plist-get c :file) (plist-get task :worktree)))
    (goto-char (point-min))
    (forward-line (1- (plist-get c :start)))))

(defun godel-review-list-edit ()
  "Edit the comment at point."
  (interactive)
  (let ((c (godel-review--list-comment))
        (id godel-review--list-task))
    (godel-compose (plist-get (godel-task-get id) :repo) 'comment
                   (format "Edit comment on %s:%s" (plist-get c :file) (plist-get c :start))
                   (lambda (text) (godel-review-update-comment id (plist-get c :id) :text text))
                   (plist-get c :text))))

(defun godel-review-list-accept ()
  "Accept the reviewer finding at point: it becomes a pending comment."
  (interactive)
  (let ((fid (or (magit-section-value-if 'godel-finding) (user-error "godel: no finding here"))))
    (require 'godel-reviewer)
    (godel-reviewer-accept godel-review--list-task fid)))

(defun godel-review-list-delete ()
  "Delete the comment at point, or drop the reviewer finding at point."
  (interactive)
  (if-let* ((fid (magit-section-value-if 'godel-finding)))
      (progn (require 'godel-reviewer)
             (godel-reviewer-drop godel-review--list-task fid))
  (let ((c (godel-review--list-comment)))
    (when (y-or-n-p (format "Delete the comment on %s:%s? " (plist-get c :file) (plist-get c :start)))
      (godel-review-delete-comment godel-review--list-task (plist-get c :id))))))

(defun godel-review-list-send ()
  "Send the pending comments (after editing them as one message)."
  (interactive)
  (godel-review-send (godel-task-get godel-review--list-task)))

(defun godel-review--refresh-lists (task change)
  (unless (eq change 'deleted)
    (when-let* ((buffer (get-buffer (godel-review--list-buffer-name (plist-get task :id)))))
      (with-current-buffer buffer (godel-review-list-refresh)))))

(add-hook 'godel-event-functions #'godel-review--refresh-lists)

;;;; Asking the author

(defun godel-review-question-text (task question &optional target)
  "QUESTION for TASK's author, with the code at TARGET (FILE START END) quoted."
  (if (not target)
      question
    (pcase-let ((`(,file ,start ,end) target))
      (format "About `%s` %s:\n```\n%s\n```\n%s"
              file (if (= start end) (format "line %d" start) (format "lines %d-%d" start end))
              (or (godel-review--file-lines task file start end) "")
              question))))

(defun godel-review-ask (task &optional target)
  "Ask TASK's author a question; TARGET (FILE START END) attaches that code."
  (let ((id (plist-get task :id)))
    (godel-compose
     (plist-get task :repo) 'question
     (if target
         (format "Question for %s about %s:%s" id (car target)
                 (if (= (nth 1 target) (nth 2 target)) (nth 1 target)
                   (format "%d-%d" (nth 1 target) (nth 2 target))))
       (format "Question for %s" id))
     (lambda (question)
       (godel-review--send-to-author
        (godel-task-get id) (godel-review-question-text (godel-task-get id) question target)
        (lambda () (message "godel: question sent to %s" id))))
     nil
     (and target
          (mapconcat (lambda (l) (concat "> " l))
                     (split-string (or (godel-review--file-lines task (car target)
                                                                 (nth 1 target) (nth 2 target))
                                       "")
                                   "\n")
                     "\n")))))

;;;; History

(defun godel-review-history (task)
  "Show the feedback rounds sent to TASK's author, newest first."
  (let ((rounds (plist-get task :review-rounds)))
    (unless rounds (user-error "godel: no feedback sent for %s yet" (plist-get task :id)))
    (with-current-buffer (get-buffer-create (format "*godel-review-history: %s*" (plist-get task :id)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (r (reverse rounds))
          (insert (propertize (format "Round %d · %s\n" (plist-get r :round)
                                      (format-time-string "%Y-%m-%d %H:%M" (plist-get r :time)))
                              'face 'magit-section-heading)
                  (plist-get r :text) "\n\n")))
      (special-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; Menu

(defvar godel-review--menu-task nil "Task ID the open review menu acts on.")
(defvar godel-review--menu-target nil
  "(FILE START END) of the region when the review menu was opened, if any.")

(defun godel-review--menu-target ()
  (or (godel-task-get godel-review--menu-task) (user-error "godel: no task")))

(transient-define-prefix godel-review-menu-prefix ()
  "Review a godel task."
  [:description (lambda () (format "Review %s" (plist-get (godel-review--menu-target) :title)))
   ["Look"
    ("d" "diff with comments" (lambda () (interactive) (godel-review-diff (godel-review--menu-target))))
    ("l" "list comments" (lambda () (interactive) (godel-review-list (godel-review--menu-target))))
    ("h" "feedback history" (lambda () (interactive) (godel-review-history (godel-review--menu-target))))]
   ["Talk to the author"
    ("s" "send pending comments" (lambda () (interactive) (godel-review-send (godel-review--menu-target))))
    ("a" (lambda () (interactive) (godel-review-ask (godel-review--menu-target) godel-review--menu-target))
     :description godel-review--ask-description)]
   ["Independent review"
    ("r" "review (Not Diamond)"
     (lambda () (interactive) (require 'godel-reviewer) (godel-reviewer-run (godel-review--menu-target))))
    ("R" "review on a model..."
     (lambda () (interactive) (require 'godel-reviewer)
       (godel-reviewer-run (godel-review--menu-target) (godel-reviewer--read-model))))
    ("w" "walkthrough (Not Diamond)"
     (lambda () (interactive) (require 'godel-reviewer)
       (godel-reviewer-walkthrough (godel-review--menu-target))))
    ("W" "walkthrough on a model..."
     (lambda () (interactive) (require 'godel-reviewer)
       (godel-reviewer-walkthrough (godel-review--menu-target) (godel-reviewer--read-model))))]
   ["Ship"
    ("P" "commit, push, open PR" (lambda () (interactive) (require 'godel-ship)
                                  (godel-ship (godel-review--menu-target))))]])

(defun godel-review--ask-description ()
  (if godel-review--menu-target
      (format "ask about %s:%s" (car godel-review--menu-target) (nth 1 godel-review--menu-target))
    "ask a question"))

;;;###autoload
(defun godel-review-menu (&optional task)
  "Open the review menu for TASK (default: the task this buffer belongs to)."
  (interactive)
  (let ((task (or task (godel-review--buffer-task)
                  (and (derived-mode-p 'godel-review-mode) (godel-task-get godel-review--list-task))
                  (user-error "godel: not in a task (use v on the board)"))))
    (setq godel-review--menu-task (plist-get task :id)
          ;; A region in this task's diff or file: questions can be about it.
          godel-review--menu-target (and (use-region-p)
                                         (eq (godel-review--buffer-task) task)
                                         (ignore-errors (godel-review--target))))
    (transient-setup 'godel-review-menu-prefix)))

(provide 'godel-review)
;;; godel-review.el ends here
