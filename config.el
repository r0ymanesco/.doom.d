;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "r0ymanesco"
      user-mail-address "r0ymanesco@gmail.com")


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-challenger-deep)


;; This fixes the pound key issue on UK MacOS keyboards
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; conda
(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
(custom-set-variables
 '(conda-anaconda-home "~/miniconda3/"))


;; Magit
(require 'magit-todos)
(magit-todos-mode)


;; Ranger
(ranger-override-dired-mode t)


;; Org
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(setq org-log-done 'time)
(setq org-agenda-files (list "~/Dropbox/org" "~/Dropbox/org/bibliography/notes" "~/Dropbox/org/bibliography"))

(after! org-capture
  (add-to-list 'org-capture-templates
               '("u" "Quick note" entry
                 (file+headline "~/Dropbox/org/notes.org" "Quick Notes")
                 "* %t %?\n%a" :kill-buffer t)
               )
)

(after! org-capture
  (add-to-list 'org-capture-templates
               '("v" "Quick todo" entry
                 (file+headline "~/Dropbox/org/notes.org" "Quick Notes")
                 "* TODO %t %?" :kill-buffer t)
               )
)

(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "STRT(s)" "HOLD(h)" "MEETING(m)" "READ(r)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
        )
)
;; (setq org-todo-keyword-faces
;;       '(("TODO" . "green") ("STRT" . "blue") ("HOLD" . "orange") ("MEETING" . "yellow")
;;         ("KILL" . "red")))

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; doesn't work for checkboxes
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
;; Source: https://orgmode.org/manual/Breaking-Down-Tasks.html

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  )

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks t
           org-appear-autoentities t
           org-appear-autosubmarkers t))


;; Ebib
(use-package ebib
:bind (("C-c e" . ebib))
:ensure t
:config
(setq ebib-index-columns
(quote(
("Entry Key" 30 t)
("Author/Editor" 20 nil)
("Year" 6 t)
("Title" 60 t)
("keywords" 20 t))))
(setq ebib-bibtex-dialect 'biblatex
      ebib-preload-bib-files '("~/Dropbox/org/bibliography/bib/main.bib")
      ebib-file-search-dirs '("~/Sync/papers/pdf")
      ebib-notes-directory "~/Dropbox/org/bibliography/notes"
      ebib-use-timestamp t
      ebib-timestamp-format "%d.%m.%Y"
      ebib-index-window-size 50
      ebib-keywords (expand-file-name "~/Dropbox/org/bibliography/keywords.txt")
      ebib-keywords-add-new-to-canonical 0
      ebib-keywords-save-on-exit t
      ebib-reading-list-file "~/Dropbox/org/bibliography/reading_list.org"
      ebib-reading-list-todo-marker "READ")
(if (eq system-type 'darwin)
        (add-to-list 'ebib-file-associations '("pdf" . "open -a Skim %s"))
        (add-to-list 'ebib-file-associations '("pdf" . nil)))
(advice-add 'bibtex-generate-autokey :around
              (lambda (orig-func &rest args)
                (replace-regexp-in-string ":" "" (apply orig-func args))))
(map! :map ebib-index-mode-map "B" #'ebib-biblio-import-doi)
(map! :map ebib-index-mode-map "F f" #'ebib-import-file)
(map! :map ebib-index-mode-map "C-x S" #'ebib-edit-strings)
(map! :map ebib-strings-mode-map "C-x a" #'ebib-add-string))
;; (setq ebib-window-vertical-split t)
;; (setq ebib-file-associations
;;       '(("pdf" . "open -a Skim %s")))

(require 'ebib-biblio)
(with-eval-after-load 'biblio
  (define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import))
;; (with-eval-after-load 'ebib
;;   (define-key ebib-index-mode-map (kbd "B") 'ebib-biblio-import-doi)
;;   (define-key ebib-index-mode-map (kbd "F f") 'ebib-import-file)
;;   (define-key ebib-index-mode-map (kbd "C-x S") 'ebib-edit-strings)
;;   (define-key ebib-strings-mode-map (kbd "C-x a") 'ebib-add-string))

;; (defun acm-pdf-url (doi)
;;   "Retrieve a DOI pdf from the ACM."
;;   (concat "https://dl.acm.org/doi/pdf/" doi))

;; (defun ieee-pdf-url (doi)
;;   "Retrieve a DOI pdf from the IEEE."
;;   (when (string-match "\\.\\([0-9]*\\)$" doi)
;;     (let ((doi-bit (match-string 1 doi)))
;;       (concat "https://ieeexplore.ieee.org/stampPDF/getPDF.jsp?tp=&arnumber=" doi-bit "&ref="))))

;; (defun springer-pdf-url (doi)
;;   "Retrieve a DOI pdf from the Springer."
;;   (concat "https://link.springer.com/content/pdf/" doi ".pdf"))

;; (defun arxiv-pdf-url (eprint)
;;   "Download an arXiv pdf based on it's EPRINT number."
;;   (concat "https://arxiv.org/pdf/" eprint ".pdf"))

;; (defun download-pdf-from-doi (key &optional doi publisher eprint journal organization url)
;;   "Download pdf from DOI with KEY name."
;;   (let ((pub  (or publisher ""))
;;         (epr  (or eprint ""))
;;         (jour (or journal ""))
;;         (org  (or organization ""))
;;         (link (or url "")))
;;     (url-copy-file (cond
;;                     ((not doi) link)
;;                     ((or (string-match "ACM" (s-upcase pub))
;;                          (string-match "association for computing machinery" (s-downcase pub)))
;;                      (acm-pdf-url doi))
;;                     ((string-match "arxiv" (s-downcase pub))
;;                      (arxiv-pdf-url epr))
;;                     ((or (string-match "IEEE" (s-upcase pub))
;;                          (string-match "IEEE" (s-upcase jour))
;;                          (string-match "IEEE" (s-upcase org)))
;;                      (ieee-pdf-url doi))
;;                     ((string-match "springer" (s-downcase pub))
;;                      (springer-pdf-url doi))
;;                     (t (error "Cannot possibly find the PDF any other way")))
;;                    (concat (car ebib-file-search-dirs) "/" key ".pdf"))))

;; (defun download-pdf-from-link (link key)
;;   (url-copy-file link
;;                  (concat (car ebib-file-search-dirs) "/" key ".pdf")))

;; (defun download-pdf-from-downloads (key)
;;   (copy-file (concat "~/Downloads/" key ".pdf")
;;              (concat (car ebib-file-search-dirs) "/" key ".pdf") t))

;; (defun get-bib-from-doi (doi)
;;   "Get the bibtex from DOI."
;;   (shell-command (concat "curl -L -H \"Accept: application/x-bibtex; charset=utf-8\" "
;;                          "https://doi.org/" doi)))

;; (defun ebib-download-pdf-from-doi ()
;;   "Download a PDF for the current entry."
;;   (interactive)
;;   (let* ((key (ebib--get-key-at-point))
;;          (doi (ebib-get-field-value "doi" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (publisher (ebib-get-field-value "publisher" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (eprinttype (ebib-get-field-value "eprinttype" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (eprint (ebib-get-field-value "eprint" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (journal (ebib-get-field-value "journal" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (journaltitle (ebib-get-field-value "journaltitle" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (organization (ebib-get-field-value "organization" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (url (ebib-get-field-value "url" key ebib--cur-db 'noerror 'unbraced 'xref)))
;;     (unless key
;;       (error "[Ebib] No key assigned to entry"))
;;     (download-pdf-from-doi key doi (or publisher eprinttype) eprint (or journal journaltitle) organization url)))

;; (defun ebib-check-file ()
;;   "Download a PDF for the current entry."
;;   (interactive)
;;   (let ((key (ebib--get-key-at-point)))
;;     (unless (file-exists-p (concat (car ebib-file-search-dirs) "/" key ".pdf"))
;;       (error "[Ebib] No PDF found"))))


;; Zettelkasten
(use-package org-zettelkasten
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-zettelkasten-mode))
(setq org-zettelkasten-directory "~/Dropbox/org")

(use-package zettelkasten
  :ensure t
  :config
  (zettelkasten-mode t))

(defun org-zettelkasten-search-current-id ()
    "Use `counsel-rg' to search for the current ID in all files."
    (interactive)
    (let ((current-id (org-entry-get nil "CUSTOM_ID")))
      (counsel-rg (concat "#" current-id) org-zettelkasten-directory "-g *.org" "ID: ")))

(defun r0ymanesco/incr-id (ident)
  (let* ((ident-list (append nil ident nil))
         (last-ident (last ident-list)))
    (setcar last-ident (+ (car last-ident) 1))
    (concat ident-list)))

(defun r0ymanesco/branch-id (ident)
  (if (string-match-p ".*[0-9]$" ident)
      (concat ident "a")
    (concat ident "1")))

(defun r0ymanesco/org-zettelkasten-create (incr newheading)
  (let* ((current-id (org-entry-get nil "CUSTOM_ID"))
         (next-id (funcall incr current-id)))
    (funcall newheading)
    (org-set-property "CUSTOM_ID" next-id)))

(defun org-zettelkasten-create-heading ()
  (r0ymanesco/org-zettelkasten-create
   'r0ymanesco/incr-id 'org-insert-heading))

(defun org-zettelkasten-create-subheading ()
  (r0ymanesco/org-zettelkasten-create
   'r0ymanesco/branch-id '(lambda () (org-insert-subheading ""))))

(defun org-zettelkasten-create-dwim ()
  (interactive)
  (let ((current-point (save-excursion
                         (org-back-to-heading)
                         (point)))
        (next-point (save-excursion
                      (org-forward-heading-same-level 1 t)
                      (point))))
    (if (= current-point next-point)
        (org-zettelkasten-create-heading)
        (org-zettelkasten-create-subheading))))


;; Tabs
(use-package centaur-tabs
  :config
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-c h" . centaur-tabs-backward)
  ("C-c l" . centaur-tabs-forward)
  ("C-c d" . centaur-tabs--kill-this-buffer-dont-ask)
  ("C-c n" . centaur-tabs--create-new-tab)
  )


;; Rust
(add-to-list 'exec-path "~/.cargo/bin")

;; (with-eval-after-load 'rust-mode
;;   (define-key rust-mode-map (kbd "C-c C-r") 'my-cargo-run))

;; custom method to allow keyboard input
;; doesn't work...
(defun my-cargo-run ()
  "Build and run Rust code."
  (interactive)
  (rustic-cargo-run)
  (let (
      (orig-win (selected-window))
      (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible))
    )
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (select-window orig-win)
  )
)


;; Elfeed (RSS reader)
;; data is stored in ~/.elfeed
;; (global-set-key (kbd "SPC o E") 'elfeed)
(map! :leader :prefix "o" :desc "Elfeed" "E" 'elfeed)
(use-package elfeed
:ensure t
:bind
("C-c r" . elfeed-update))
(setq elfeed-feeds
      '(
        ;; programming
        ;; ("https://www.reddit.com/r/emacs.rss" emacs)
        ;; ("https://www.reddit.com/r/rust.rss" rust)
        ;; economics
        ("https://noahpinion.substack.com/feed" noahpinion)
        ("https://braddelong.substack.com/feed" braddelong)
        ("https://theovershoot.co/feed" theovershoot)
        ("https://fallows.substack.com/feed" breakingnews)
        ("https://www.platformer.news/feed" platformer)
        ))

(setq-default elfeed-search-filter "@7-days-ago +unread")
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)


;; ssh-deploy
;; - prefix = C-c C-z, f = forced upload, u = upload, d = download, x = diff, t = terminal, b = browse, h = shell
;; NOTE: hydra bindings don't work
;; Need to setup passwordless copy to remote and project .dir-locals.el config
;; NOTE: removed after-save hook to avoid non-project save errors, seems to work w/o. add back if problems occur
(use-package ssh-deploy
       :ensure t
       :hook ((find-file . ssh-deploy-find-file))
              ;; (after-save . ssh-deploy-after-save)
       :config
       (ssh-deploy-line-mode) ;; If you want mode-line feature
       (ssh-deploy-add-menu) ;; If you want menu-bar feature
     )


;; Tramp
;; (use-package anaconda-mode
;;   :config
;;   (setq anaconda-mode-localhost-address "localhost")
;;   )
;; (setq anaconda-mode-localhost-address "localhost")
;; (defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
;;       "Start a program in a subprocess.  Return the process object for it.
;; Similar to `start-process-shell-command', but calls `start-file-process'."
;;       ;; On remote hosts, the local `shell-file-name' might be useless.
;;       (let ((command (mapconcat 'identity args " ")))
;;         (funcall start-file-process-shell-command name buffer command)))

;; (advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)

;; (use-package lsp-mode
;;   :hook (python-mode . lsp)
;;   :config
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
;;                     :major-modes '(python-mode)
;;                     :remote? t
;;                     :server-id 'pyls-remote)))

;; Set tramp remote path
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/")
;; (require 'tramp)
;; (add-to-list 'Info-directory-list "/usr/share/info/")
;; (add-to-list 'tramp-remote-path "~/miniconda/bin")
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; (connection-local-set-profile-variables 'iblis
;;    '((tramp-remote-path . ("~/miniconda/bin" tramp-default-remote-path))))
;; (connection-local-set-profiles
;;    '(:application tramp :user "tt2114" :machine "iblis") 'iblis)


;; (define-key evil-motion-state-map (kbd "C-e") nil)
;; (define-key smartparens-mode-map (kbd "C-e") #'sp-up-sexp)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
