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

(defcustom org-mini-files nil
  "Files that will be affected by org-mini-mode."
  :type '(string)
  :group 'agenda)

(defun org-mini--enable-minor-mode ()
  "Activate org-mini when current buffer is on org-mini-files."
  (when (and buffer-file-name
             (member buffer-file-name org-mini-files))
    (org-mini-mode 1)))

(define-minor-mode org-mini-mode
  "Toggle org-mini-mode and display org mode as is."
  :lighter "org-mini"
  (if org-mini-mode
      (setq-local face-remapping-alist
                  (mapcar (lambda (face) `(,face (:height ,(face-attribute 'default :height)) ,face)) org-level-faces))
    (setq-local face-remapping-alist nil)))

(add-hook 'find-file-hook #'org-mini--enable-minor-mode)

(provide 'org-mini)
;;; org-mini.el ends here
