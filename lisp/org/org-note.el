;;; org/org-note.el --- Make org more like note -*- lexical-binding: t; -*-
(require 'org)
(require 'mixed-pitch)
(require 'visual-fill-column)

(define-minor-mode org-note-mode
  "Toggle org-note-mode and display org mode as note."
  :lighter "org-note"
  (if org-note-mode
      (progn
        (setq-local visual-fill-column-width 96
                    visual-fill-column-center-text t)
        (pushnew! mixed-pitch-fixed-pitch-faces
            'solaire-line-number-face
            'org-document-info
            'org-date
            'org-footnote
            'org-special-keyword
            'org-property-value
            'org-ref-cite-face
            'org-tag
            'org-todo-keyword-todo
            'org-todo-keyword-habt
            'org-todo-keyword-done
            'org-todo-keyword-wait
            'org-todo-keyword-kill
            'org-todo-keyword-outd
            'org-todo
            'org-done
            'font-lock-comment-face)
        (visual-fill-column-mode 1)
        (mixed-pitch-mode 1))
    (progn
      (setq-local visual-fill-column-width nil)
      (mixed-pitch-mode 0)
      (visual-fill-column-mode 0))))

(provide 'org-note)
