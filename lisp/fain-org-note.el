;;; fain-org-note.el --- Make org more like note -*- lexical-binding: t; -*-
(require 'org)
(require 'mixed-pitch)
(require 'visual-fill-column)

(define-minor-mode fain-org-note-mode
  "Toggle fain-org-note-mode and display org mode as note."
  :lighter "fain-org-note"
  (if fain-org-note-mode
      (progn
        (setq-local visual-fill-column-width 96
                    visual-fill-column-center-text t)
        (mixed-pitch-mode 1)
        (visual-fill-column-mode 1))
    (progn
      (setq-local visual-fill-column-width nil)
      (mixed-pitch-mode 0)
      (visual-fill-column-mode 0))))

(provide 'fain-org-note)
