;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Fikri Rahmat Nurhidayat"
      user-mail-address "fikrirnurhidayat@gmail.com")

;; == UI
;; Configuring emacs UI is kinda shit tbh.
;; === Fonts
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-\serif-font' -- for the `fixed-pitch-serif' face
(setq doom-font (font-spec :family "Iosevka Fixed" :size 16)
      doom-big-font (font-spec :size 24)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16 :weight 'normal)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "Iosevka Etoile" :weight 'normal))

;; Set slant on some font
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; === Theme
;; Nord because it's cool, I guess.
(setq doom-theme 'doom-nord-aurora)

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(set-face-background 'fringe (face-attribute 'default :background))

;; === Line Numbers
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; == Programming
(setq inferior-lisp-program "ros -Q run") ; tell emacs what to run on the repl when we're working on lisp

;; == Keybinding
(setq doom-leader-key "SPC") ; Setup leader key

;; == Org
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/home/fain/Documents/org/"                  ; set org directory inside documents
      org-clock-sound "/home/fain/Documents/bababooey.wav"       ; set the bell sound
      truncate-string-ellipsis "…"                      ; truncate elipsis
      org-ellipsis " ▾ "                                ; set elipsis
      org-use-property-inheritance t                    ; it's convenient to have properties inherited
      org-log-done 'time                                ; having the time a item is done sounds convenient
      org-list-allow-alphabetical t                     ; have a. A. a) A) list bullets
      org-export-in-background t                        ; run export processes in external emacs process
      org-hide-emphasis-markers t                       ; hide emphasis marker
      org-fold-catch-invisible-edits 'smart)            ; try not to accidently do weird stuff in invisible regions

(setq org-roam-directory (concat org-directory "roams/")
      +org-agenda-directory (concat org-directory "agenda/"))

;; Hide number on org-mode
(add-hook! org-mode (display-line-numbers-mode 0))

;; Run this after org-mode
(after! org
  (setq org-agenda-custom-commands
        '(("k" "Di Kantor" tags-todo "@kantor"
           ((org-agenda-overriding-header "Kantor")))
          ("h" "Di Rumah" tags-todo "@rumah"
           ((org-agenda-overriding-header "Rumah")))
          ("o" "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-deadline-warning-days 365)))
            (todo "TODO"
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files '("~/Documents/org/agenda/INBOX.org"))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files '("~/Documents/org/agenda/PROJECTS.org"
                                       "~/Documents/org/agenda/MAYBE.org"
                                       "~/Documents/org/agenda/NEXT.org"))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files '("~/Documents/org/agenda/PROJECTS.org"))))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-off Tasks")
                   (org-agenda-files '("~/Documents/org/agenda/NEXT.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
            nil))))
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "VOID(c@/!)")))
  (setq org-agenda-files (mapcar (lambda (file) (concat +org-agenda-directory file)) '("PROJECTS.org" "INBOX.org" "MAYBE.org" "NEXT.org")))
  (setq org-refile-targets '(("PROJECTS.org" :maxlevel . 3)
                             ("MAYBE.org" :level . 1)
                             ("NEXT.org" :maxlevel . 2)
                             ("BOOK.org" :maxlevel . 3)))
  (setq org-capture-templates '(("i" "Inbox" entry (file "~/Documents/org/agenda/INBOX.org")
                                 "* TODO %i%?")
                                ("l" "Link" entry (file "~/Documents/org/agenda/INBOX.org")
                                 "* TODO %(org-cliplink-capture)" :immediate-finish t)
                                ("c" "Capture Website" entry (file "~/Documents/org/agenda/INBOX.org")
                                 "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t))))

(use-package! org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("•")))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  (setq-local display-line-numbers-mode nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

;; Org Roam
(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-capture-templates
   '(("d" "Default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-complete-everywhere t))

(defun +org-present-hook ()
  (setq-local visual-fill-column-width 192
      visual-fill-column-center-text t)

  ;; TODO: Map family using variable
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 2.0) org-document-title)
                                     (org-table (:height 0.5 :family "Iosevka") org-table)
                                     (org-code (:height 1.0 :family "Iosevka") org-code)
                                     (org-verbatim (:height 1.0 :family "Iosevka") org-verbatim)
                                     (org-block (:height 1.0 :family "Iosevka") org-block)
                                     (org-block-begin-line (:height 0.7) org-block-begin-line)
                                     (org-block-end-line (:height 0.7) org-block-end-line)))
  (display-line-numbers-mode 0)
  (setq header-line-format " ")
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (hide-mode-line-mode 1)
  (org-display-inline-images))

(defun +org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (visual-fill-column-mode 0)
  (org-indent-mode 1)
  (hide-mode-line-mode 0)
  (org-superstar-restart)
  (org-mode-restart)
  (org-remove-inline-images))

(defun +org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-fold-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-fold-show-children))

(use-package! org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . org-present-next)
         ("C-c C-k" . org-present-prev))
  :hook ((org-present-mode . +org-present-hook)
         (org-present-mode-quit . +org-present-quit-hook))
  :init
    (add-hook 'org-present-after-navigate-functions '+org-present-prepare-slide))

;; == Markdown
(add-hook! markdown-mode (display-line-numbers-mode 0)) ; Hide line number on markdown mode

;; Add empty space and run writeroom-mode
(add-hook! gfm-view-mode
  (setq header-line-format " ")
  (setq-local face-remapping-alist '(header-line (:height 4.0) variable-pitch))
  (writeroom-mode 1))

(use-package! markdown-mode
  :init (setq markdown-list-item-bullets '("•" "•" "•" "•" "•" "•" "•"))
  (setq markdown-header-scaling t)
  (setq markdown-header-scaling-values '(2.0 1.5 1.25 1.125 1.0 1.0)))

(use-package! projectile
  :init
  (when (and (file-directory-p "~/Works/Repositories") (file-directory-p "~/Repositories"))
    (setq projectile-project-search-path '("~/Works/Repositories" "~/Repositories"))))

(defvar +zen-serif-p t
  "Whether to use a serifed font with `mixed-pitch-mode'.")

(defvar +zen-org-level-scale '((org-level-1 . 1.25)
                               (org-level-2 . 1.125)
                               (org-level-3 . 1.0625)
                               (org-level-4 . 1.0)
                               (org-level-5 . 1.0)
                               (org-level-6 . 1.0)
                               (org-level-7 . 1.0)
                               (org-level-8 . 1.0))
  "Org level size remap.")

(defun +zen-prose-org-h ()
  "Reformat the current Org buffer appearance for prose."
  (when (eq major-mode 'org-mode)
    (setq display-line-numbers nil
          visual-fill-column-width 72
          org-adapt-indentation t)
    (when (featurep 'org-superstar)
      (setq-local org-superstar-remove-leading-stars t)
      (org-superstar-restart))
    (setq-local face-remapping-alist (mapcar (lambda (face) `(,(car face) (:height ,(cdr face))  ,(car face))) +zen-org-level-scale))
    (setq +zen-text-scale 1.619
          +zen--original-org-indent-mode-p org-indent-mode
          +zen--original-org-pretty-table-mode-p (bound-and-true-p org-pretty-table-mode))))

(defun +zen-nonprose-org-h ()
  "Reverse the effect of `+zen-prose-org'."
  (when (eq major-mode 'org-mode)
    (setq-local face-remapping-alist nil)
    (when (featurep 'org-superstar)
      (setq-local org-superstar-remove-leading-stars nil)
      (org-superstar-restart))))

(after! writeroom-mode
  (setq +zen--original-org-indent-mode-p t)
  (setq +zen--original-org-pretty-table-mode-p nil)
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-superstar-headline-bullets-list
            'org-superstar-remove-leading-stars)
  (add-hook 'writeroom-mode-enable-hook #'+zen-prose-org-h))
  (add-hook 'writeroom-mode-disable-hook #'+zen-nonprose-org-h)

(map! :leader
      :desc "Present"
      "t p" #'org-present)

(use-package! doom-modeline
  :init
  (setq doom-modeline-height 48)
  (setq doom-modeline-bar-width 8)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-window-width-limit nil)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-number-limit 99)
  (setq doom-modeline-lsp nil))

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun +get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun +map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun +get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar '+map-line-to-status-char status-lines)))))

(defun +get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun +eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (+get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground ,(doom-color 'magenta)))
     (propertize " 🙚 " 'face `(:foreground ,(doom-color 'fg)))
     (propertize (+get-prompt-path) 'face `(:foreground ,(doom-color 'cyan)))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground ,(doom-color 'fg)))
        (propertize (concat " " current-branch) 'face `(:foreground ,(doom-color 'red)))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground ,(doom-color 'cyan)))
        (propertize package-version 'face `(:foreground ,(doom-color 'cyan)))))
     (propertize " • " 'face `(:foreground ,(doom-color 'fg)))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground ,(doom-color 'base4)))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground ,(doom-color 'success)))
       (propertize "\nλ" 'face `(:foreground ,(doom-color 'success))))
     (propertize " " 'face `(:foreground ,(doom-color 'fg))))))

(defun +eshell-configure ()
  (evil-collection-eshell-setup)
  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      '+eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))

(add-hook 'eshell-first-time-mode-hook #'+eshell-configure)
(setq eshell-directory-name "~/.cache/eshell/"
      eshell-aliases-file (expand-file-name "~/.cache/eshell/alias"))

(after! protobuf-mode
  (set-formatter! 'protofmt "buf format -w" :modes '(protobuf-mode)))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(load! "lisp/org-mini/org-mini")

(setq org-mini-files org-agenda-files)

(use-package! org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(modify-all-frames-parameters
 '((internal-border-width . 32)))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-fold-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")
