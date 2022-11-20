;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;bidi                 ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout               ; auie,ctsrnm is the superior home row

       :completion
       company                 ; the ultimate code completion backend
       (vertico +icons)        ; the search engine of the future

       :ui
       deft                    ; notational velocity for Emacs
       doom                    ; what makes DOOM look the way it does
       hl-todo                 ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (ligatures +extra)      ; ligatures and symbols to make your code pretty again
       modeline                ; snazzy, Atom-inspired modeline, plus API
       ophints                 ; highlight the region an operation acts on
       (popup +defaults)       ; tame sudden yet inevitable temporary windows
       treemacs                ; a project drawer, like neotree but cooler
       unicode                 ; extended unicode support for various languages
       workspaces              ; tab emulation, persistence & separate workspaces
       zen                     ; distraction-free coding or writing

       :editor
       (evil +everywhere)      ; come to the dark side, we have cookies
       file-templates          ; auto-snippets for empty files
       fold                    ; (nigh) universal code folding
       (format +onsave)        ; automated prettiness
       lispy                   ; vim for lisp, for people who don't like vim
       snippets                ; my elves. They type so I don't have to

       :emacs
       dired                   ; making dired pretty [functional]
       undo                    ; persistent, smarter undo for your inevitable mistakes
       vc                      ; version-control and Emacs, sitting in a tree #+end_src

       :term
       eshell                  ; the elisp shell that works everywhere

       :checkers
       syntax                  ; tasing you for every semicolon you forget

       :tools
       (eval +overlay)     ; run code, run (also, repls)
       gist                ; interacting with github gists
       lookup              ; navigate your code and its documentation
       lsp                 ; M-x vscode
       magit               ; a git porcelain for Emacs
       make                ; run make tasks from Emacs
       pass                ; password manager for nerds
       rgb                 ; creating color strings
       tree-sitter         ; syntax and parsing, sitting in a tree...

       :os
       (:if IS-MAC macos)      ; improve compatibility with macOS
       ;;tty                  ; improve the terminal Emacs experience

       :lang
       (cc +lsp)               ; C > C++ == 1
       clojure                 ; java with a lisp
       common-lisp             ; if you've seen one lisp, you've seen them all
       data                    ; config/data formats
       (dart +flutter +lsp)    ; paint ui and not much else
       emacs-lisp              ; drown in parentheses
       (go +lsp)               ; the hipster dialect
       json                    ; At least it ain't XML
       (java +lsp)             ; the poster child for carpal tunnel syndrome
       javascript              ; all(hope(abandon(ye(who(enter(here))))))
       julia                   ; a better, faster MATLAB
       kotlin                  ; a better, slicker Java(Script)
       latex                   ; writing papers in Emacs has never been so fun
       lua                     ; one-based indices? one-based indices
       markdown                ; writing docs for people to ignore
       (org                    ; organize your plain life in plain text
       +roam2                  ; whoa a second brain?
       +appear                 ; reduce the unnecessary text
       +present                ; present like a chad
       +cliplink)              ; nuclear code
       racket                  ; a DSL for DSLs
       rest                    ; Emacs as a REST client
       (ruby +rails)           ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)             ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (scheme +guile)         ; a fully conniving family of lisps
       sh                      ; she sells {ba,z,fi}sh shells on the C xor
       yaml                    ; JSON, but readable

       :email
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere           ; *leave* Emacs!? You must be joking
       ;;irc                  ; how neckbeards socialize
       ;;(rss +org)           ; emacs as an RSS reader
       ;;twitter              ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings))
