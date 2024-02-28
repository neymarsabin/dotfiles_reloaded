;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       :completion
       (company +childframe)            ; the ultimate code completion backend
       vertico           ; the search engine of the future
       ;; (ivy +fuzzy +prescient +icons +childframe)

       :ui
       (emoji +unicode)  ; 🙂
       indent-guides     ; highlighted indent columns
       (modeline +light)         ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       ;; (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;; (vc-gutter +pretty) ; vcs diff in the fringe

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       snippets          ; my elves. They type so I don't have to

       :emacs
       electric          ; smarter, keyword-based electric-indent
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :tools
       docker
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       (lsp +peek)                ; M-x vscode
       magit             ; a git porcelain for Emacs
       ;; pass           ; password manager for nerds
       tree-sitter       ; syntax and parsing, sitting in a tree...
       (terraform )

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS

       :lang
       emacs-lisp        ; drown in parentheses
       (go +lsp)
       json              ; At least it ain't XML
       (javascript +lsp)
       markdown          ; writing docs for people to ignore
       (org
        +pretty
        +journal
        +present
        +hugo
        +lsp)              ; organize your plain life in plain text
       ;; sh              ; she sells {ba,z,fi}sh shells on the C xor
       solidity
       web               ; the tubes
       yaml
       (lua +lsp)

       :config
       (default +bindings +smartparens))
