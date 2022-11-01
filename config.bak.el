;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "Fikri Rahmat Nurhidayat"
      user-mail-address "fikrirnurhidayat@gmail.com")

(setq doom-theme 'doom-nord)

(setq doom-font (font-spec :family "Iosevka Term" :size 16)
      doom-big-font (font-spec :family "Iosevka Term" :size 24)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16 :weight 'light)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "Iosevka Etoile"))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/Documents/org/"            ; tempat penyimpanan berkas-berkas dari org
      org-roam-directory "~/Documents/roams/"     ; tempat penyimpanan berkas-berkas dari org roam
      org-clock-sound "~/Documents/bababooey.wav" ; suara notifikasi bababooey
      truncate-string-ellipsis "…"                ; pengganti elipsis
      org-ellipsis " ▾ "                          ; string elipsis yang ditampilkan ketika suatu bagian dilipat
      org-use-property-inheritance t              ; mewarisi properti dari tajuk yang lebih tinggi
      org-log-done 'time                          ; mencatat kapan suatu item selesai
      org-list-allow-alphabetical t               ; menggunakan alfabet sebagai prefiks di masing-masing item di dalam daftar
      org-export-in-background t                  ; mengekspor tanpa mengganggu pekerjaan lain
      org-hide-emphasis-markers t                 ; menyembunyikan emphasis di dalam berkas org
      org-fold-catch-invisible-edits 'smart)      ; berhati-hati ketika ada benda yang tidak terlihat

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
  ;; biar keliatan dari awal, `org-appear--set-elements'
  ;; perlu dijalanin setelah hook lain udah dijalanin.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package! org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . org-present-next)
         ("C-c C-k" . org-present-prev))
  :hook ((org-present-mode . +org-present-hook)
         (org-present-mode-quit . +org-present-quit-hook))
  :init
    (add-hook 'org-present-after-navigate-functions '+org-present-prepare-slide))

(defun +org-present-hook ()
  (setq-local visual-fill-column-width 192
      visual-fill-column-center-text t)

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
