;;; org/org-note.el --- Make org more like note -*- lexical-binding: t; -*-
(require 'org)
(require 'mixed-pitch)
(require 'visual-fill-column)

(define-minor-mode org-note-mode
  "Toggle org-note-mode and display org mode as note."
  :lighter "org-note"
  (if org-note-mode
      (progn
        (setq-local visual-fill-column-width 72
                    visual-fill-column-center-text t)
        (visual-fill-column-mode 1))
    (progn
      (setq-local visual-fill-column-width nil)
      (visual-fill-column-mode 0))))

(provide 'org-note)
