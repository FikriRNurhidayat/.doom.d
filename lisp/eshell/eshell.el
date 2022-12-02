;;; $DOOMDIR/lisp/eshell/eshell.el -*- lexical-binding: t; -*-
(defun read-file (file-path)
  "Read file with temporary buffer."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

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

(defun +eshell-prompt ()
  (let ((current-branch (magit-get-current-branch)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground ,(doom-color 'magenta)))
     (propertize " • " 'face `(:foreground ,(doom-color 'fg)))
     (propertize (+get-prompt-path) 'face `(:foreground ,(doom-color 'cyan)))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground ,(doom-color 'fg)))
        (propertize (concat " " current-branch) 'face `(:foreground ,(doom-color 'red)))))
     (propertize " • " 'face `(:foreground ,(doom-color 'fg)))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground ,(doom-color 'base4)))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground ,(doom-color 'success)))
       (propertize "\nλ" 'face `(:foreground ,(doom-color 'success))))
     (propertize " " 'face `(:foreground ,(doom-color 'fg))))))

(defun +get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun +map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun +get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar '+map-line-to-status-char status-lines)))))
