;; -*- no-byte-compile: t; -*-

;; Org mode related stuff
(package! org-pretty-table :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "7bd68b420d3402826fea16ee5099d04aa9879b78")
(package! org-present)
(package! ox-jira)
(package! ox-clip)
(package! ox-slack)
(package! ox-gfm)
(package! exec-path-from-shell)
(unpin! org-roam)
(package! org-roam-ui)

(package! xterm-color)
(package! protobuf-mode)
(package! ripgrep)
(package! rainbow-mode)
(package! exwm)
(package! good-scroll)
(package! solaire-mode :disable t)
