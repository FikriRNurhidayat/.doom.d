;;; org-mini.el --- Make org be mini -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Fikri Rahmat Nurhidayat
;;
;; Author: Fikri Rahmat Nurhidayat <fikrirnurhidayat@gmail.com>
;; Maintainer: Fikri Rahmat Nurhidayat <fikrirnurhidayat@gmail.com>
;; Created: November 15, 2022
;; Modified: November 15, 2022
;; Version: 0.0.1
;; Keywords: abbrev outlines
;; Homepage: https://github.com/fikrirnurhidayat/org-mini
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'org)
(require 'face-remap)

(define-minor-mode org-mini-mode
  "Toggle org-mini-mode and display org mode as is."
  :lighter "org-mini"
  (if org-mini-mode
      (setq-local face-remapping-alist
                  (mapcar (lambda (face) `(,face (:height ,(face-attribute 'default :height)) ,face)) org-level-faces))
    (setq-local face-remapping-alist nil)))

(provide 'org-mini)
;;; org-mini.el ends here
