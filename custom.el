;;; custom.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Fikri Rahmat Nurhidayat

;; Author: Fikri Rahmat Nurhidayat <fikrirnurhidayat@gmail.com>
;; Keywords:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(htmlize))
 '(smtpmail-smtp-server "smtp.google.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#2E3440"))))
 '(doom-modeline-bar-inactive ((t (:background "#2E3440"))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-keyword-face ((t (:slant italic))))
 '(fringe ((t (:foreground "#2E3440"))))
 '(mode-line ((t (:background "#2E3440"))))
 '(mode-line-inactive ((t (:background "#2E3440"))))
 '(window-divider ((t (:foreground "#2E3440"))))
 '(window-divider-first-pixel ((t (:foreground "#2E3440"))))
 '(window-divider-last-pixel ((t (:foreground "#2E3440")))))
(put 'projectile-ripgrep 'disabled nil)
