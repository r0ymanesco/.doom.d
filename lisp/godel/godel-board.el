;;; godel-board.el --- The godel board -*- lexical-binding: t -*-

;;; Commentary:
;; A magit-section buffer listing every task, grouped by repo (alphabetical)
;; and then by state.  The same mode draws two views: the full board
;; (`godel-board', SPC G a) and the compact sidebar (godel-sidebar.el,
;; SPC G o).  Both redraw themselves (debounced) whenever a task changes.
;; See PLAN.md, "Board" and "Sidebar".
;;
;;   RET  open the task's workspace     n  new task
;;   TAB  show details                  m  message the agent
;;   R    restart/resume (new model?)   x  tear down (keeps the branch)
;;   d    diff with comments (C)        gr refresh
;;   q    quit (hides the sidebar)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit-section)
(require 'godel-task)
(require 'godel-models)
(require 'godel-agent)
(require 'godel-worktree)

(declare-function godel-new-task "godel-new" (&optional repo))
(declare-function godel-review-diff "godel-review" (task))
(declare-function godel-new--read-model "godel-new" (prompt initial history))
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide" (buffer))
(declare-function +workspace-switch "autoload/workspaces" (name &optional auto-create-p))
(declare-function +workspace-exists-p "autoload/workspaces" (name))
(declare-function +workspace-kill "autoload/workspaces" (workspace &optional inhibit-kill-p))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function +workspace-current-name "autoload/workspaces" ())
(declare-function godel-sidebar-hide "godel-sidebar" ())
(declare-function godel-sidebar-sync "godel-sidebar" (&rest _))
(declare-function indent-bars-mode "indent-bars" (&optional arg))
(defvar +indent-guides-inhibit-functions)

(defconst godel-board-buffer-name "*godel*")
(defconst godel-sidebar-buffer-name "*godel-sidebar*")

(defcustom godel-sidebar-width 40
  "Width of the sidebar window, in columns."
  :group 'godel
  :type 'integer)

(defconst godel-board-groups
  '(("Needs attention" needs-attention model-mismatch)
    ("Working" working)
    ("Setting up" setting-up)
    ("Needs review" needs-review)
    ("Idle" idle)
    ("Stopped" stopped))
  "Board groups in display order: (LABEL STATE...).")

(defconst godel-board-icons
  '((needs-attention . "●") (model-mismatch . "✗") (working . "◐")
    (setting-up . "…") (needs-review . "✓") (idle . "○") (stopped . "■")))

;; One face per state.  The colour is the foreground of the theme's ANSI
;; colour for that state (Doom themes map these to their palette), looked up
;; when drawing so it follows theme changes.  Only the foreground: the
;; `ansi-color-*' faces also set a background, which would turn text into
;; solid blocks.  Anything set on a `godel-state-*' face (e.g. with
;; `custom-set-faces!') takes priority over the theme colour.
(defface godel-state-needs-attention '((t :weight bold))
  "Tasks that need you (red)." :group 'godel)
(defface godel-state-model-mismatch '((t :weight bold))
  "Tasks that started on the wrong model (magenta)." :group 'godel)
(defface godel-state-working '((t))
  "Tasks the agent is working on (yellow)." :group 'godel)
(defface godel-state-setting-up '((t))
  "Tasks being set up: worktree, starting Claude (blue)." :group 'godel)
(defface godel-state-needs-review '((t))
  "Tasks ready for review (green)." :group 'godel)
(defface godel-state-idle '((t :inherit default))
  "Idle tasks (the normal text colour)." :group 'godel)
(defface godel-state-stopped '((t :inherit shadow))
  "Tasks whose session has ended (grey)." :group 'godel)
(defface godel-board-dim '((t :inherit shadow)) "Secondary text." :group 'godel)

(defconst godel-board--state-colours
  '((needs-attention . red) (model-mismatch . magenta) (working . yellow)
    (setting-up . blue) (needs-review . green))
  "ANSI colour for each state; states not listed use their face only.")

(defun godel-board--state-face (state)
  "Face for STATE: its `godel-state-*' face, over the theme's ANSI colour."
  (let* ((state (or state 'idle))
         (face (intern (format "godel-state-%s" state)))
         (colour (alist-get state godel-board--state-colours))
         (fg (and colour (face-foreground (intern (format "ansi-color-%s" colour)) nil t))))
    (if fg (list face (list :foreground fg)) face)))

(defun godel-board--paint (string face)
  "STRING with FACE.  Uses `font-lock-face' too: font-lock (on in the
board) removes plain `face' properties when it fontifies."
  (propertize string 'face face 'font-lock-face face))

(defun godel-board--age (time)
  (if (not time) ""
    (let ((s (- (float-time) time)))
      (cond ((< s 60) (format "%ds" s))
            ((< s 3600) (format "%dm" (/ s 60)))
            ((< s 86400) (format "%dh" (/ s 3600)))
            (t (format "%dd" (/ s 86400)))))))

(defun godel-board--model-label (task)
  (let ((m (godel-models-find (plist-get task :model))))
    (if (or (null m) (godel-models-router-p m))
        (or (plist-get m :name) (plist-get task :model) "?")
      (format "%s/%s" (plist-get m :name) (or (plist-get task :effort) "?")))))

(defun godel-board--repo-name (task)
  (file-name-nondirectory (directory-file-name (plist-get task :repo))))

;;;; Mode

(defvar-keymap godel-board-mode-map
  :parent magit-section-mode-map
  "RET" #'godel-board-visit
  "n" #'godel-board-new
  "m" #'godel-board-message
  "R" #'godel-board-restart
  "x" #'godel-board-teardown
  "d" #'godel-board-diff
  "g r" #'godel-board-refresh
  "q" #'godel-board-quit)

(defun godel-board--inhibit-indent-guides-p ()
  "For Doom's `+indent-guides-inhibit-functions': no guides on the board."
  (derived-mode-p 'godel-board-mode))

(define-derived-mode godel-board-mode magit-section-mode "godel"
  "The godel board: every task, grouped by repo and state."
  (setq-local revert-buffer-function (lambda (&rest _) (godel-board-refresh)))
  (setq truncate-lines t)
  ;; Doom turns indent guides on after any major mode change unless one of
  ;; these functions says no; they'd draw bars down the sidebar.
  (when (boundp '+indent-guides-inhibit-functions)
    (add-hook '+indent-guides-inhibit-functions #'godel-board--inhibit-indent-guides-p)))

(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) godel-board-mode-map
    (kbd "RET") #'godel-board-visit
    "n" #'godel-board-new
    "m" #'godel-board-message
    "R" #'godel-board-restart
    "x" #'godel-board-teardown
    "d" #'godel-board-diff
    "gr" #'godel-board-refresh
    "q" #'godel-board-quit))

;;;; Rendering

(defvar-local godel-board--compact nil "Non-nil in the sidebar view.")
(defvar-local godel-board--expanded nil "IDs of tasks whose details are shown.")
(defvar-local godel-board--collapsed-repos nil "Repos whose sections are collapsed.")

(defun godel-board--state-rank (state)
  (or (cl-position-if (lambda (group) (memq state (cdr group))) godel-board-groups) 99))

(defun godel-board--by-repo (tasks)
  "Group TASKS by repo: ((REPO . TASKS) ...), repos sorted by name then path."
  (let (groups)
    (dolist (task tasks)
      (push task (alist-get (plist-get task :repo) groups nil nil #'equal)))
    (cl-sort (mapcar (lambda (g) (cons (car g) (nreverse (cdr g)))) groups)
             (lambda (a b)
               (let ((na (file-name-nondirectory (directory-file-name a)))
                     (nb (file-name-nondirectory (directory-file-name b))))
                 (if (string= na nb) (string< a b) (string< na nb))))
             :key #'car)))

(defconst godel-board--count-icons
  '(("●" godel-state-needs-attention needs-attention model-mismatch)
    ("◐" godel-state-working working setting-up)
    ("✓" godel-state-needs-review needs-review)
    ("○" godel-state-idle idle)
    ("■" godel-state-stopped stopped)))

(defun godel-board--counts (tasks)
  "Counts per state for TASKS, like \"●1 ◐2 ✓1\"."
  (mapconcat #'identity
             (delq nil (mapcar (lambda (c)
                                 (let ((n (cl-count-if (lambda (task) (memq (plist-get task :state)
                                                                          (cddr c)))
                                                       tasks)))
                                   (when (> n 0)
                                     (godel-board--paint (format "%s%d" (car c) n) (cadr c)))))
                               godel-board--count-icons))
             " "))

(defun godel-board--current-task-id ()
  "ID of the task whose workspace is current, if any."
  (when (fboundp '+workspace-current-name)
    (plist-get (godel-task-by-slug (ignore-errors (+workspace-current-name))) :id)))

(defun godel-board--icon (task)
  (let ((state (plist-get task :state)))
    (godel-board--paint (alist-get state godel-board-icons "?") (godel-board--state-face state))))

(defun godel-board--details (task indent)
  (let ((width (if godel-board--compact (- godel-sidebar-width indent 2) 1000)))
    (dolist (row `(("id" ,(plist-get task :id))
                   ("branch" ,(format "%s (from %s)" (plist-get task :branch) (plist-get task :base)))
                   ("worktree" ,(abbreviate-file-name (plist-get task :worktree)))
                   ("model" ,(format "%s%s" (godel-board--model-label task)
                                     (if-let* ((a (plist-get task :model-actual)))
                                         (format "  (running %s)" a) "")))
                   ("changes" ,(plist-get task :diffstat))
                   ("comments" ,(godel-board--comment-summary task))
                   ("last message" ,(plist-get task :last-message))
                   ("session" ,(if (godel-agent-live-p task) "running" "not running"))))
      (when (cadr row)
        (if godel-board--compact
            (insert (godel-board--paint (format "%s%s: " (make-string indent ?\s) (car row)) 'godel-board-dim)
                    (truncate-string-to-width (cadr row) (max 10 (- width (length (car row)))) nil nil "…")
                    "\n")
          (insert (godel-board--paint (format "%s%-13s" (make-string indent ?\s) (car row)) 'godel-board-dim)
                  (cadr row) "\n"))))))

(defun godel-board--comment-summary (task)
  "Like \"2 pending, 1 sent\", or nil if TASK has no review comments."
  (let ((counts nil))
    (dolist (c (plist-get task :comments))
      (cl-incf (alist-get (plist-get c :state) counts 0)))
    (when counts
      (mapconcat (lambda (st) (format "%d %s" (alist-get st counts) st))
                 (cl-remove-if-not (lambda (st) (alist-get st counts))
                                   '(pending sent addressed outdated))
                 ", "))))

(defun godel-board--insert-task (task)
  "Insert TASK as a full-board row."
  (let ((id (plist-get task :id))
        (face (godel-board--state-face (plist-get task :state))))
    (magit-insert-section (godel-task id (not (member id godel-board--expanded)))
      (magit-insert-heading
        (format "      %s %-32s %-14s %-50s %s"
                (godel-board--icon task)
                (godel-board--paint (truncate-string-to-width (plist-get task :title) 32 nil nil "…")
                            'magit-section-secondary-heading)
                (truncate-string-to-width (godel-board--model-label task) 14 nil nil "…")
                (godel-board--paint (truncate-string-to-width (or (plist-get task :last-event) "") 50 nil nil "…")
                            face)
                (godel-board--paint (godel-board--age (or (plist-get task :updated) (plist-get task :created)))
                            'godel-board-dim)))
      (godel-board--details task 8))))

(defun godel-board--insert-task-compact (task current)
  "Insert TASK as a sidebar row; CURRENT is the ID of the task you're in."
  (let* ((id (plist-get task :id))
         (state (plist-get task :state))
         (age (godel-board--age (or (plist-get task :updated) (plist-get task :created))))
         ;; Marker, icon and spaces take 4 columns, the age plus a space
         ;; more, and magit adds "…" after a collapsed row; keep a margin.
         (title-width (max 8 (- godel-sidebar-width 4 (length age) 1 3)))
         (attention (memq state '(needs-attention model-mismatch))))
    (magit-insert-section (godel-task id (not (member id godel-board--expanded)))
      (magit-insert-heading
        (concat (if (equal id current) (godel-board--paint "▶" 'godel-state-working) " ")
                (godel-board--icon task) " "
                (godel-board--paint (truncate-string-to-width (plist-get task :title) title-width nil ?\s "…")
                            (if (equal id current) 'magit-section-highlight
                                    'magit-section-secondary-heading))
                " " (godel-board--paint age 'godel-board-dim)
                (when attention
                  (concat "\n    "
                          (godel-board--paint (truncate-string-to-width (or (plist-get task :last-event) "")
                                                                (- godel-sidebar-width 8) nil nil "…")
                                      'godel-state-needs-attention)))))
      (godel-board--details task 4))))

(defun godel-board--insert-header (tasks repos)
  (if godel-board--compact
      (insert (godel-board--paint " godel" 'magit-section-heading)
              "  " (godel-board--counts tasks) "\n")
    (insert (godel-board--paint (format "godel · %d task%s in %d repo%s"
                                (length tasks) (if (= 1 (length tasks)) "" "s")
                                (length repos) (if (= 1 (length repos)) "" "s"))
                        'magit-section-heading)
            (godel-board--paint "      n new · RET open · m message · R restart · x tear down · gr refresh\n\n"
                        'godel-board-dim))))

(defun godel-board--render ()
  (let* ((inhibit-read-only t)
         (tasks (godel-task-all))
         (repos (godel-board--by-repo tasks))
         (current (and godel-board--compact (godel-board--current-task-id))))
    (erase-buffer)
    (magit-insert-section (godel-root)
      (godel-board--insert-header tasks repos)
      (when (null tasks)
        (insert (if godel-board--compact " No tasks.  n creates one.\n"
                  "  No tasks yet.  Press n to create one.\n")))
      (pcase-dolist (`(,repo . ,rtasks) repos)
        (magit-insert-section (godel-repo repo (member repo godel-board--collapsed-repos))
          (let ((name (file-name-nondirectory (directory-file-name repo))))
            (magit-insert-heading
              (if godel-board--compact
                  (godel-board--paint name 'magit-section-heading)
                (concat (godel-board--paint name 'magit-section-heading) "  "
                        (godel-board--paint (abbreviate-file-name (directory-file-name repo))
                                    'godel-board-dim)
                        "    " (godel-board--counts rtasks)))))
          (if godel-board--compact
              (dolist (task (cl-stable-sort (copy-sequence rtasks) #'<
                                            :key (lambda (task) (godel-board--state-rank
                                                                 (plist-get task :state)))))
                (godel-board--insert-task-compact task current))
            (pcase-dolist (`(,label . ,states) godel-board-groups)
              (let ((group (cl-remove-if-not (lambda (task) (memq (plist-get task :state) states))
                                             rtasks)))
                (when group
                  (magit-insert-section (godel-group (cons repo (car states)))
                    (magit-insert-heading (format "    %s (%d)" label (length group)))
                    (mapc #'godel-board--insert-task group)))))
            (insert "\n")))))
    ;; magit-insert-section only records HIDE; apply it (magit's own buffers
    ;; do this during their refresh).
    (magit-map-sections (lambda (s) (when (oref s hidden) (magit-section-hide s))))))

(defun godel-board--task-id-at-point ()
  (magit-section-value-if 'godel-task))

(defun godel-board--repo-at-point ()
  "Repo of the task or repo heading at point, if any."
  (or (when-let* ((id (godel-board--task-id-at-point)))
        (plist-get (godel-task-get id) :repo))
      (magit-section-value-if 'godel-repo)
      (car-safe (magit-section-value-if 'godel-group))))

(defun godel-board--remember-visibility ()
  (let (expanded collapsed)
    (when magit-root-section
      (magit-map-sections
       (lambda (s)
         (pcase (oref s type)
           ('godel-task (unless (oref s hidden) (push (oref s value) expanded)))
           ('godel-repo (when (oref s hidden) (push (oref s value) collapsed)))))))
    (setq godel-board--expanded expanded
          godel-board--collapsed-repos collapsed)))

(defun godel-board--refresh-buffer (buffer)
  "Redraw BUFFER, keeping point on the same task or repo."
  (with-current-buffer buffer
    (let* ((section (magit-current-section))
           (type (and section (oref section type)))
           (value (and section (oref section value)))
           (line (line-number-at-pos))
           (windows (get-buffer-window-list buffer nil t)))
      (godel-board--remember-visibility)
      (godel-board--render)
      (goto-char (point-min))
      (or (and (memq type '(godel-task godel-repo))
               (cl-block found
                 (magit-map-sections
                  (lambda (s)
                    (when (and (eq (oref s type) type) (equal (oref s value) value))
                      (goto-char (oref s start))
                      (cl-return-from found t))))))
          (forward-line (1- line)))
      (dolist (w windows) (set-window-point w (point))))))

(defun godel-board-refresh ()
  "Redraw the board and the sidebar."
  (interactive)
  (dolist (name (list godel-board-buffer-name godel-sidebar-buffer-name))
    (when-let* ((buffer (get-buffer name)))
      (godel-board--refresh-buffer buffer))))

(defvar godel-board--timer nil)

(defun godel-board--schedule (&rest _)
  "Redraw the board and sidebar soon (debounced), if either exists."
  (when (and (or (get-buffer godel-board-buffer-name) (get-buffer godel-sidebar-buffer-name))
             (not godel-board--timer))
    (setq godel-board--timer
          (run-with-timer 0.2 nil (lambda ()
                                    (setq godel-board--timer nil)
                                    (godel-board-refresh))))))

(defun godel-board-buffer (&optional compact)
  "Return the board buffer (or with COMPACT, the sidebar's), drawn."
  (godel-task-load-all)
  (let ((buffer (get-buffer-create (if compact godel-sidebar-buffer-name godel-board-buffer-name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'godel-board-mode) (godel-board-mode))
      (when (bound-and-true-p indent-bars-mode) (indent-bars-mode -1))
      (setq godel-board--compact compact)
      (godel-board--refresh-buffer buffer))
    buffer))

(defcustom godel-board-workspace "Godel"
  "Name of the Doom workspace that holds the board."
  :group 'godel
  :type 'string)

;;;###autoload
(defun godel-board ()
  "Show the board in its own workspace (`godel-board-workspace').
The workspace is created on first use; afterwards this switches back to
it.  Without Doom workspaces, the board opens in the selected window."
  (interactive)
  (let ((buffer (godel-board-buffer)))
    (if (not (fboundp '+workspace-switch))
        (switch-to-buffer buffer)
      (+workspace-switch godel-board-workspace t)
      (if-let* ((window (get-buffer-window buffer)))
          (select-window window)
        (select-window (window-main-window))
        (delete-other-windows)
        (switch-to-buffer buffer)))))

;;;; Commands

(defun godel-board--task ()
  (or (godel-task-get (godel-board--task-id-at-point))
      (user-error "godel: no task at point")))

(defun godel-board-new ()
  "Create a task in the repo at point (task or repo heading), or ask."
  (interactive)
  (godel-new-task (godel-board--repo-at-point)))

(defun godel-board-quit ()
  "Quit the board; in the sidebar, hide it (everywhere, until SPC G o)."
  (interactive)
  (if godel-board--compact (godel-sidebar-hide) (quit-window)))

(defun godel-board-visit ()
  "Open the task's workspace: magit-status on the left, Claude on the right."
  (interactive)
  (let* ((task (godel-board--task))
         (buffer (godel-agent-buffer task)))
    (when (fboundp '+workspace-switch)
      (+workspace-switch (plist-get task :slug) t))
    ;; From the sidebar the selected window is a side window, which can't be
    ;; made the only window.
    (select-window (window-main-window))
    (delete-other-windows)
    (if (file-directory-p (plist-get task :worktree))
        (magit-status-setup-buffer (plist-get task :worktree))
      (dired (plist-get task :repo)))
    ;; Re-read: switching workspace and drawing magit can run timers, and
    ;; one of them may already have started resuming this task.
    (setq task (godel-task-get (plist-get task :id))
          buffer (godel-agent-buffer task))
    (cond (buffer (godel-board--display-session buffer t))
          ((godel-agent-starting-p task)
           (godel-board--show-session task t))
          ((godel-agent-resumable-p task)
           (message "godel: resuming %s's Claude session" (plist-get task :id))
           (godel-board--show-session task t))
          (t (message "godel: no session to resume; R on the board starts one")))
    (when (fboundp 'godel-sidebar-sync) (godel-sidebar-sync))))

(defun godel-board--display-session (buffer select)
  "Show the session BUFFER on the right; with SELECT, select its window."
  (when (fboundp 'claude-code-ide--display-buffer-in-side-window)
    (let ((window (or (get-buffer-window buffer)
                      (claude-code-ide--display-buffer-in-side-window buffer))))
      (when (and select window) (select-window window))
      window)))

(defun godel-board--in-workspace-p (task)
  "Non-nil if TASK's workspace is current (or there are no workspaces)."
  (or (not (fboundp '+workspace-current-name))
      (equal (ignore-errors (+workspace-current-name)) (plist-get task :slug))))

(defun godel-board--show-session (task &optional select)
  "Resume TASK's session if needed and show it, if you're still in its workspace."
  (godel-agent-ensure-session
   task (lambda (buffer)
          (when (and buffer (godel-board--in-workspace-p task))
            (godel-board--display-session buffer select)))))

(defun godel-board-on-workspace-activated (&rest _)
  "After a workspace switch: resume the task's session if it isn't running.
Sessions end with Emacs, so this brings a task back however you reach its
workspace."
  (run-with-timer
   0 nil
   (lambda ()
     (when-let* ((task (and (fboundp '+workspace-current-name)
                            (godel-task-by-slug (ignore-errors (+workspace-current-name))))))
       (when (godel-agent-resumable-p task)
         (godel-board--show-session task))))))

(defun godel-board-diff ()
  "Show the task's changes against its base branch, with comments."
  (interactive)
  (require 'godel-review)
  (let ((task (godel-board--task)))
    (when godel-board--compact (select-window (window-main-window)))
    (godel-review-diff task)))

(defun godel-board-message ()
  "Send a message to the task's agent."
  (interactive)
  (let ((task (godel-board--task)))
    (godel-agent-send task (read-string (format "Message %s: " (plist-get task :id))))))

(defun godel-board-restart ()
  "Restart the task's session, resuming the conversation.
Asks for the model and effort, defaulting to the current ones."
  (interactive)
  (require 'godel-new)
  (when (godel-agent-starting-p (godel-board--task))
    (user-error "godel: %s is still starting; wait for it, then R"
                (plist-get (godel-board--task) :id)))
  (let* ((task (godel-board--task))
         (id (plist-get task :id))
         (godel-default-model (plist-get task :model))
         (model (godel-new--read-model "Model: " nil nil))
         (entry (godel-models-find model))
         (effort (unless (godel-models-router-p entry)
                   (completing-read "Effort: " (plist-get entry :efforts) nil t nil nil
                                    (or (plist-get task :effort) godel-default-effort))))
         ;; Resume if the session ever started (it has a transcript);
         ;; reusing the ID for a fresh start would be rejected.
         (resume (plist-get task :transcript)))
    (when (or (not (godel-agent-live-p task))
              (y-or-n-p (format "Stop %s's running session and restart it? " id)))
      (godel-agent-stop task)
      (godel-task-update id :model model :effort effort)
      ;; Let claude-code-ide finish cleaning up the old session first.
      (run-with-timer 0.5 nil (lambda () (godel-agent-start (godel-task-get id) resume))))))

(defun godel-board-teardown ()
  "Stop the session, remove the worktree and forget the task.
The branch is kept."
  (interactive)
  (let* ((task (godel-board--task))
         (id (plist-get task :id)))
    (godel-worktree-check
     task
     (lambda (counts)
       (when (y-or-n-p
              (format "Tear down %s?%s The branch %s is kept. "
                      id
                      (if (> (car counts) 0)
                          (format " %d uncommitted file(s) will be LOST." (car counts))
                        "")
                      (concat (plist-get task :branch)
                              (if (> (cdr counts) 0)
                                  (format " (%d unpushed commit(s))" (cdr counts)) ""))))
         (godel-agent-stop task)
         (when (and (fboundp '+workspace-exists-p) (+workspace-exists-p (plist-get task :slug)))
           (+workspace-kill (plist-get task :slug)))
         (godel-task-update id :state 'setting-up :last-event "tearing down")
         (godel-worktree-remove
          task (lambda (err)
                 (if err
                     (godel-task-update id :state 'stopped :last-event err)
                   (godel-task-delete id)
                   (message "godel: %s torn down; branch %s kept"
                            id (plist-get task :branch))))))))))

(add-hook 'godel-event-functions #'godel-board--schedule)

(provide 'godel-board)
;;; godel-board.el ends here
