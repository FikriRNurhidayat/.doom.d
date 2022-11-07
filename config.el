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
(setq doom-font (font-spec :family "Iosevka Term" :size 16)
      doom-big-font (font-spec :family "Iosevka Term" :size 24)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16 :weight 'light)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "Iosevka Etoile"))

;; Set slant on some font
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; === Theme
;; Nord because it's cool, I guess.
(setq doom-theme 'doom-nord)

;; === Line Numbers
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; ==Programming
(setq inferior-lisp-program "ros -Q run") ; tell emacs what to run on the repl when we're working on lisp

;; == Keybinding
(setq doom-leader-key "SPC") ; Setup leader key

;; == Org
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/"            ; set org directory inside documents
      org-clock-sound "~/Documents/bababooey.wav" ; set the bell sound
      truncate-string-ellipsis "…"                ; truncate elipsis
      org-ellipsis " ▾ "                          ; set elipsis
      org-roam-directory "~/Documents/roams/"     ; set roam directory on different folder for org
      org-use-property-inheritance t              ; it's convenient to have properties inherited
      org-log-done 'time                          ; having the time a item is done sounds convenient
      org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
      org-export-in-background t                  ; run export processes in external emacs process
      org-hide-emphasis-markers t                 ; hide emphasis marker
      org-fold-catch-invisible-edits 'smart)      ; try not to accidently do weird stuff in invisible regions

;; Hide number on org-mode
(add-hook! org-mode (display-line-numbers-mode 0))

;; Run this after org-mode
(after! org
  (setq org-agenda-custom-commands
        '(("o" "Di Kantor" tags-todo "@kantor"
           ((org-agenda-overriding-header "Kantor")
            (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
          ("h" "Di Rumah" tags-todo "@rumah"
           ((org-agenda-overriding-header "Rumah")
            (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

  ;; Org Agenda
  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keywords-for-agenda '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-agenda-files '("~/Documents/gtd/Box.org"
                           "~/Documents/gtd/Projects.org"
                           "~/Documents/gtd/Tickler.org"))

  (setq org-refile-targets '(("~/Documents/gtd/Projects.org" :maxlevel . 3)
                             ("~/Documents/gtd/Maybe.org" :level . 1)
                             ("~/Documents/gtd/Tickler.org" :maxlevel . 2)))

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file "~/Documents/gtd/Box.org")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file "~/Documents/gtd/Tickler.org")
                                 "* %i%? \n %U"))))

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
      :unnarrowed t)
     ("m" "Meeting Note" plain
      "* Participants\n\n- %?\n\n* Goals\n\n- Meeting Goals goes here!\n\n* Discussion Topics\n\n- Lorem ipsum\n\n* Action Items\n\n** TODO Write the meeting goals"
      :if-new (file+head "work/mom/%<%Y%m%d%H%M%S>.${slug}.org" "#+title: ${title}\n#+filetags: :@kantor:@mom:\n")
      :unnarrowed t)
     ("p" "Presentation" plain
      "%?"
      :if-new (file+head "work/presentation/%<%Y%m%d%H%M%S>.${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-complete-everywhere t))

(defun +org-present-hook ()
  (setq-local visual-fill-column-width 192
      visual-fill-column-center-text t)

  ;; TODO: Map family using variable
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     ;; (org-level-1 (:height 1.0) org-level-1)
                                     ;; (org-level-2 (:height 1.0) org-level-2)
                                     ;; (org-level-3 (:height 1.0) org-level-3)
                                     ;; (org-level-4 (:height 1.0) org-level-4)
                                     ;; (org-level-5 (:height 1.0) org-level-5)
                                     ;; (org-level-6 (:height 1.0) org-level-6)
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

(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))

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

(after! writeroom-mode
  (setq +zen--original-org-indent-mode-p t)
  (setq +zen--original-org-pretty-table-mode-p nil)
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-superstar-headline-bullets-list
            'org-superstar-remove-leading-stars)
  (add-hook 'writeroom-mode-enable-hook
            (defun +zen-prose-org-h ()
              "Reformat the current Org buffer appearance for prose."
              (when (eq major-mode 'org-mode)
                (setq display-line-numbers nil
                      visual-fill-column-width 96
                      org-adapt-indentation nil)
                (when (featurep 'org-superstar)
                  (setq-local org-superstar-remove-leading-stars t)
                  (org-superstar-restart))
                (org-indent-mode 0)
                (setq +zen-text-scale 1.5
                      +zen--original-org-indent-mode-p org-indent-mode
                      +zen--original-org-pretty-table-mode-p (bound-and-true-p org-pretty-table-mode)))))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-nonprose-org-h ()
              "Reverse the effect of `+zen-prose-org'."
              (when (eq major-mode 'org-mode)
                (when (featurep 'org-superstar)
                  (org-superstar-restart))
                (when +zen--original-org-indent-mode-p (org-indent-mode 1))))))

;; (defvar fancy-splash-image-template
;;   (expand-file-name "misc/splash-images/emacs-e-template.svg" doom-user-dir)
;;   "Default template svg used for the splash image, with substitutions from ")

;; (defvar fancy-splash-sizes
;;   `((:height 300 :min-height 50 :padding (0 . 2))
;;     (:height 250 :min-height 42 :padding (2 . 4))
;;     (:height 200 :min-height 35 :padding (3 . 3))
;;     (:height 150 :min-height 28 :padding (3 . 3))
;;     (:height 100 :min-height 20 :padding (2 . 2))
;;     (:height 75  :min-height 15 :padding (2 . 1))
;;     (:height 50  :min-height 10 :padding (1 . 0))
;;     (:height 1   :min-height 0  :padding (0 . 0)))
;;   "list of plists with the following properties
;;   :height the height of the image
;;   :min-height minimum `frame-height' for image
;;   :padding `+doom-dashboard-banner-padding' (top . bottom) to apply
;;   :template non-default template file
;;   :file file to use instead of template")

;; (defvar fancy-splash-template-colours
;;   '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
;;   "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

;; (unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
;;   (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

;; (defun fancy-splash-filename (theme-name height)
;;   (expand-file-name (concat (file-name-as-directory "theme-splashes")
;;                             theme-name
;;                             "-" (number-to-string height) ".svg")
;;                     doom-cache-dir))

;; (defun fancy-splash-clear-cache ()
;;   "Delete all cached fancy splash images"
;;   (interactive)
;;   (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
;;   (message "Cache cleared!"))

;; (defun fancy-splash-generate-image (template height)
;;   "Read TEMPLATE and create an image if HEIGHT with colour substitutions as
;;    described by `fancy-splash-template-colours' for the current theme"
;;   (with-temp-buffer
;;     (insert-file-contents template)
;;     (re-search-forward "$height" nil t)
;;     (replace-match (number-to-string height) nil nil)
;;     (dolist (substitution fancy-splash-template-colours)
;;       (goto-char (point-min))
;;       (while (re-search-forward (car substitution) nil t)
;;         (replace-match (doom-color (cdr substitution)) nil nil)))
;;     (write-region nil nil
;;                   (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

;; (defun fancy-splash-generate-images ()
;;   "Perform `fancy-splash-generate-image' in bulk"
;;   (dolist (size fancy-splash-sizes)
;;     (unless (plist-get size :file)
;;       (fancy-splash-generate-image (or (plist-get size :template)
;;                                        fancy-splash-image-template)
;;                                    (plist-get size :height)))))

;; (defun ensure-theme-splash-images-exist (&optional height)
;;   (unless (file-exists-p (fancy-splash-filename
;;                           (symbol-name doom-theme)
;;                           (or height
;;                               (plist-get (car fancy-splash-sizes) :height))))
;;     (fancy-splash-generate-images)))

;; (defun get-appropriate-splash ()
;;   (let ((height (frame-height)))
;;     (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
;;              fancy-splash-sizes)))

;; (setq fancy-splash-last-size nil)
;; (setq fancy-splash-last-theme nil)
;; (defun set-appropriate-splash (&rest _)
;;   (let ((appropriate-image (get-appropriate-splash)))
;;     (unless (and (equal appropriate-image fancy-splash-last-size)
;;                  (equal doom-theme fancy-splash-last-theme)))
;;     (unless (plist-get appropriate-image :file)
;;       (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
;;     (setq fancy-splash-image
;;           (or (plist-get appropriate-image :file)
;;               (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
;;     (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
;;     (setq fancy-splash-last-size appropriate-image)
;;     (setq fancy-splash-last-theme doom-theme)
;;     (+doom-dashboard-reload)))

;; (add-hook 'window-size-change-functions #'set-appropriate-splash)
;; (add-hook 'doom-load-theme-hook #'set-appropriate-splash)

;; (defvar splash-phrase-source-folder
;;   (expand-file-name "misc/splash-phrases" doom-user-dir)
;;   "A folder of text files with a fun phrase on each line.")

;; (defvar splash-phrase-sources
;;   (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
;;          (sets (delete-dups (mapcar
;;                              (lambda (file)
;;                                (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
;;                              files))))
;;     (mapcar (lambda (sset)
;;               (cons sset
;;                     (delq nil (mapcar
;;                                (lambda (file)
;;                                  (when (string-match-p (regexp-quote sset) file)
;;                                    file))
;;                                files))))
;;             sets))
;;   "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

;; (defvar splash-phrase-set
;;   (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
;;   "The default phrase set. See `splash-phrase-sources'.")

;; (defun splase-phrase-set-random-set ()
;;   "Set a new random splash phrase set."
;;   (interactive)
;;   (setq splash-phrase-set
;;         (nth (random (1- (length splash-phrase-sources)))
;;              (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
;;   (+doom-dashboard-reload t))

;; (defvar splase-phrase--cache nil)

;; (defun splash-phrase-get-from-file (file)
;;   "Fetch a random line from FILE."
;;   (let ((lines (or (cdr (assoc file splase-phrase--cache))
;;                    (cdar (push (cons file
;;                                      (with-temp-buffer
;;                                        (insert-file-contents (expand-file-name file splash-phrase-source-folder))
;;                                        (split-string (string-trim (buffer-string)) "\n")))
;;                                splase-phrase--cache)))))
;;     (nth (random (length lines)) lines)))

;; (defun splash-phrase (&optional set)
;;   "Construct a splash phrase from SET. See `splash-phrase-sources'."
;;   (mapconcat
;;    #'splash-phrase-get-from-file
;;    (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
;;    " "))

;; (defun doom-dashboard-phrase ()
;;   "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
;;   (mapconcat
;;    (lambda (line)
;;      (+doom-dashboard--center
;;       +doom-dashboard--width
;;       (with-temp-buffer
;;         (insert-text-button
;;          line
;;          'action
;;          (lambda (_) (+doom-dashboard-reload t))
;;          'face 'doom-dashboard-menu-title
;;          'mouse-face 'doom-dashboard-menu-title
;;          'help-echo "Random phrase"
;;          'follow-link t)
;;         (buffer-string))))
;;    (split-string
;;     (with-temp-buffer
;;       (insert (splash-phrase))
;;       (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
;;       (fill-region (point-min) (point-max))
;;       (buffer-string))
;;     "\n")
;;    "\n"))

;; (defadvice! doom-dashboard-widget-loaded-with-phrase ()
;;   :override #'doom-dashboard-widget-loaded
;;   (setq line-spacing 0.2)
;;   (insert
;;    "\n"
;;    (doom-dashboard-phrase)
;;    "\n"))

;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
;; (add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
;; (setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

;;
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(appendq! +ligatures-extra-symbols
          `(:checkbox      "☐"
            :pending       "◼"
            :checkedbox    "☑"
            :filetags      ""
            :list_property "∷"
            :em_dash       "—"
            :ellipses      "…"
            :arrow_right   "→"
            :arrow_left    "←"
            :title         "𝙏"
            :email         ""
            :subtitle      "𝙩"
            :author        "𝘼"
            :date          "𝘿"
            :property      "☸"
            :options       "⌥"
            :startup       "⏻"
            :macro         "𝓜"
            :html_head     "🅷"
            :html          "🅗"
            :latex_class   "🄻"
            :latex_header  "🅻"
            :beamer_header "🅑"
            :latex         "🅛"
            :attr_latex    "🄛"
            :attr_html     "🄗"
            :attr_org      "⒪"
            :begin_quote   "❝"
            :end_quote     "❞"
            :caption       "☰"
            :header        "›"
            :results       "🠶"
            :begin_export  "⏩"
            :end_export    "⏪"
            :properties    "⚙"
            :end           "∎"
            :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
            :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
            :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
            :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
            :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))

(require 'ob-js)
(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(set-ligatures! 'org-mode
  :merge t
  :filetags      "#+filetags:"
  :checkbox      "[ ]"
  :pending       "[-]"
  :checkedbox    "[X]"
  :list_property "::"
  :em_dash       "---"
  :ellipsis      "..."
  :arrow_right   "->"
  :arrow_left    "<-"
  :title         "#+title:"
  :email         "#+email:"
  :subtitle      "#+subtitle:"
  :author        "#+author:"
  :date          "#+date:"
  :property      "#+property:"
  :options       "#+options:"
  :startup       "#+startup:"
  :macro         "#+macro:"
  :html_head     "#+html_head:"
  :html          "#+html:"
  :latex_class   "#+latex_class:"
  :latex_header  "#+latex_header:"
  :beamer_header "#+beamer_header:"
  :latex         "#+latex:"
  :attr_latex    "#+attr_latex:"
  :attr_html     "#+attr_html:"
  :attr_org      "#+attr_org:"
  :begin_quote   "#+begin_quote"
  :end_quote     "#+end_quote"
  :caption       "#+caption:"
  :header        "#+header:"
  :begin_export  "#+begin_export"
  :end_export    "#+end_export"
  :results       "#+RESULTS:"
  :property      ":PROPERTIES:"
  :end           ":END:"
  :priority_a    "[#A]"
  :priority_b    "[#B]"
  :priority_c    "[#C]"
  :priority_d    "[#D]"
  :priority_e    "[#E]")
(plist-put +ligatures-extra-symbols :name "⁍")
(plist-put +ligatures-extra-symbols :begin_example "⏩")
(plist-put +ligatures-extra-symbols :end_example "⏪")

(map-delete +ligatures-extra-symbols :def)
(map-delete +ligatures-extra-symbols :composition)
(map-delete +ligatures-extra-symbols :map)
(map-delete +ligatures-extra-symbols :null)
(map-delete +ligatures-extra-symbols :true)
(map-delete +ligatures-extra-symbols :false)
(map-delete +ligatures-extra-symbols :int)
(map-delete +ligatures-extra-symbols :float)
(map-delete +ligatures-extra-symbols :str)
(map-delete +ligatures-extra-symbols :bool)
(map-delete +ligatures-extra-symbols :list)
(map-delete +ligatures-extra-symbols :not)
(map-delete +ligatures-extra-symbols :in)
(map-delete +ligatures-extra-symbols :not-in)
(map-delete +ligatures-extra-symbols :and)
(map-delete +ligatures-extra-symbols :or)
(map-delete +ligatures-extra-symbols :for)
(map-delete +ligatures-extra-symbols :some)
(map-delete +ligatures-extra-symbols :return)
(map-delete +ligatures-extra-symbols :yield)
(map-delete +ligatures-extra-symbols :union)
(map-delete +ligatures-extra-symbols :intersect)
(map-delete +ligatures-extra-symbols :diff)
(map-delete +ligatures-extra-symbols :tuple)
(map-delete +ligatures-extra-symbols :pipe)
(map-delete +ligatures-extra-symbols :dot)

(setq emojify-emoji-set "twemoji-v2")
(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀"
    ;; I just want to see this as text
    "©" "™")
  "Characters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))

(defun emojify--replace-text-with-emoji (orig-fn emoji text buffer start end &optional target)
  "Modify `emojify--propertize-text-for-emoji' to replace ascii/github emoticons with unicode emojis, on the fly."
  (if (or (not emoticon-to-emoji) (= 1 (length text)))
      (funcall orig-fn emoji text buffer start end target)
    (delete-region start end)
    (insert (ht-get emoji "unicode"))))

(define-minor-mode emoticon-to-emoji
  "Write ascii/gh emojis, and have them converted to unicode live."
  :global nil
  :init-value nil
  (if emoticon-to-emoji
      (progn
        (setq-local emojify-emoji-styles '(ascii github unicode))
        (advice-add 'emojify--propertize-text-for-emoji :around #'emojify--replace-text-with-emoji)
        (unless emojify-mode
          (emojify-turn-on-emojify-mode)))
    (setq-local emojify-emoji-styles (default-value 'emojify-emoji-styles))
    (advice-remove 'emojify--propertize-text-for-emoji #'emojify--replace-text-with-emoji)))

(dolist (face '((org-level-1 . 1.44)
                (org-level-2 . 1.25)
                (org-level-3 . 1.125)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font doom-variable-pitch-font :weight 'medium :height (cdr face)))

(map! :leader
      :desc "Present"
      "t p" #'org-present)

(use-package! doom-modeline
  :init
  (setq doom-modeline-height 48)
  (setq doom-modeline-bar-width 4)
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

;; (flycheck-define-checker buf
;;   "A Proto syntax checker using the buf.build linter. See https://docs.buf.build/lint/usage."
;;   :command ("buf" "lint" source)
;;   :error-patterns
;;   ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
;;   :modes protobuf-mode)

(use-package! lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(protobuf-mode . "protobuf"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "bufls serve")
                    :activation-fn (lsp-activate-on "protobuf")
                    :server-id 'bufls)))

(defun +exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(setq +exwm-enabled (and (eq window-system 'x)
                         (seq-contains-p command-line-args "--use-exwm")))

(use-package! exwm
  :config
  ;; Set the default number of workspaces
  (setq garbage-collection-messages nil)
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'+exwm-update-class)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (when +exwm-enabled
    (exwm-enable)))

(after! doom (doom/set-frame-opacity 97))
