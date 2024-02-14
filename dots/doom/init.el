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
       company           ; the ultimate code completion backend
       vertico           ; the search engine of the future

       :ui
       (emoji +unicode)  ; 🙂
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ophints           ; highlight the region an operation acts on
       ;; (popup +defaults)   ; tame sudden yet inevitable temporary windows
       treemacs          ; a project drawer, like neotree but cooler
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB

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

       :term
       ;; vterm             ; the best terminal emulation in Emacs

       :checkers

       :tools
       docker
       (eval +overlay)     ; run code, run (also, repls)
       ;; gist              ; interacting with github gists
       lookup              ; navigate your code and its documentation
       lsp                 ; M-x vscode
       magit             ; a git porcelain for Emacs
       ;; pass           ; password manager for nerds
       ;; terraform         ; infrastructure as code
       tree-sitter       ; syntax and parsing, sitting in a tree...

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS

       :lang
       emacs-lisp        ; drown in parentheses
       (go +lsp)
       json              ; At least it ain't XML
       (javascript +lsp)
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       (org
        +pretty
        +journal
        +present
        +hugo)              ; organize your plain life in plain text
       rust
       sh              ; she sells {ba,z,fi}sh shells on the C xor
       solidity
       web               ; the tubes
       yaml

       :app
       calendar

       :config
       (default +bindings +smartparens))
