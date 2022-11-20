;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Fikri Rahmat Nurhidayat"
      user-mail-address "fikrirnurhidayat@gmail.com")

(setq doom-font
      (font-spec :family "Iosevka Fixed" :size 20)

      doom-big-font
      (font-spec :size 32)

      doom-variable-pitch-font
      (font-spec :family "Iosevka Aile" :size 20 :weight 'normal)

      doom-unicode-font
      (font-spec :family "JuliaMono")

      doom-serif-font
      (font-spec :family "Iosevka Etoile" :weight 'normal))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-nord-aurora)

(defun +doom-remove-annoying-visual ()
  "Remove border, fringe, and so on."
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel
                  fringe))
    (custom-set-faces! `(,face :foreground ,(face-attribute 'default :background)))))

;; TODO: Find what hook should we attach so this will always be properly executed
(add-to-list 'doom-load-theme-hook '+doom-remove-annoying-visual)

(setq inhibit-message t
      echo-keystores nil
      message-log-max nil)

(use-package! doom-modeline
  :config
  (setq doom-modeline-height 48
        doom-modeline-bar-width 8
        doom-modeline-hud nil
        doom-modeline-icon t
        doom-modeline-window-width-limit nil
        doom-modeline-major-mode-icon t
        doom-modeline-number-limit 99
        doom-modeline-lsp nil))

(after! doom (custom-set-faces! `(mode-line :background ,(face-attribute 'default :background))))

(setq evil-want-fine-undo t         ; Be more granular
      auto-save-default t           ; Make sure your work is saved
      truncate-string-ellipsis "…") ; Save some precious space

(setq org-directory "/home/fain/Documents/org/")

(setq org-clock-sound "/home/fain/Documents/bababooey.wav")

(setq org-use-property-inheritance t
      org-log-done 'time
      org-list-allow-alphabetical t
      org-export-in-background t
      org-fold-catch-invisible-edits 'smart)

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

(use-package! org-modern
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "◇"))
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(setq org-hide-emphasis-markers t)

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(setq org-pretty-entities t
      org-ellipsis "…")

(setq +org-agenda-directory (concat org-directory "agenda/"))
(setq org-agenda-files (mapcar (lambda (file)
                                 (concat +org-agenda-directory file))
                               '("INBOX.org" "PROJECTS.org" "NEXT.org" "MAYBE.org")))

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "VOID(c@/!)")))

(setq org-refile-targets '(("PROJECTS.org" :maxlevel . 3)
                             ("MAYBE.org" :level . 1)
                             ("NEXT.org" :maxlevel . 2)))

(setq org-capture-templates '(("i" "Inbox" entry (file "~/Documents/org/agenda/INBOX.org") "* TODO %i%?")
                              ("w" "Website" entry (file "~/Documents/org/agenda/INBOX.org")
                                                             "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))

(setq org-agenda-tags-column 0
      org-agenda-block-separator ?─
      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")

(setq org-agenda-custom-commands
      '(("o" "Agenda"
         (
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
          )
          nil)))

(after! writeroom-mode
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width)
  (add-hook 'writeroom-mode-enable-hook #'+zen-prose-org-h))
  (add-hook 'writeroom-mode-disable-hook #'+zen-nonprose-org-h)

(defvar +zen-org-level-scale '((org-level-1 . 1.5)
                               (org-level-2 . 1.25)
                               (org-level-3 . 1.125)
                               (org-level-4 . 1.0)
                               (org-level-5 . 1.0)
                               (org-level-6 . 1.0)
                               (org-level-7 . 1.0)
                               (org-level-8 . 1.0))
  "Org level size remap.")

(defun +zen-prose-org-h ()
  "Reformat the current Org buffer appearance for prose."
  (when (eq major-mode 'org-mode)
    (setq visual-fill-column-width 64
          org-adapt-indentation t
          +zen-text-scale 1.25)
    (org-indent-mode 0)
    (setq-local face-remapping-alist (mapcar (lambda (face) `(,(car face) (:height ,(cdr face))  ,(car face))) +zen-org-level-scale))))

(defun +zen-nonprose-org-h ()
  "Reverse the effect of `+zen-prose-org'."
  (when (eq major-mode 'org-mode)
    (org-indent-mode 1)
    (setq org-adapt-indentation nil)
    (setq-local face-remapping-alist nil)))

(use-package! org-present
  :hook ((org-present-mode . +org-present-hook)
         (org-present-mode-quit . +org-present-quit-hook))
  :init
  (map! :leader :desc "Present" "t p" #'org-present)
  (add-hook 'org-present-after-navigate-functions '+org-present-prepare-slide))

(defun +org-present-hook ()
  (when writeroom-mode (writeroom-mode 0))
  (setq-local visual-fill-column-width 192
              visual-fill-column-center-text t
              header-line-format " "
              face-remapping-alist '((default (:height 2.0) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 2.0) org-document-title)
                                     (org-table (:height 0.5 :family "Iosevka") org-table)
                                     (org-code (:height 1.0 :family "Iosevka") org-code)
                                     (org-verbatim (:height 1.0 :family "Iosevka") org-verbatim)
                                     (org-block (:height 1.0 :family "Iosevka") org-block)
                                     (org-block-begin-line (:height 0.7) org-block-begin-line)
                                     (org-block-end-line (:height 0.7) org-block-end-line)))
  (display-line-numbers-mode 0)
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
  (org-overview)
  (org-fold-show-entry)
  (org-fold-show-children))

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat org-directory "roams/"))
  (org-roam-capture-templates
   '(("d" "Default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-complete-everywhere t))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(load-file (concat doom-user-dir "fain-eshell.el"))

(use-package! projectile
  :init
  (when (and (file-directory-p "~/Works/Repositories") (file-directory-p "~/Repositories"))
    (setq projectile-project-search-path '("~/Work/Repositories" "~/Repositories"))))
