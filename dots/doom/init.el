;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

(doom! :input

       :completion
       (company +childframe +icons)        ; code completion
       vertico                              ; fuzzy picker (telescope parity)

       :ui
       (emoji +unicode)
       hl-todo                              ; TODO/FIXME highlighting
       indent-guides                        ; indent-blankline parity
       (modeline +light)                    ; lualine parity
       ophints
       (popup +defaults)                    ; tame transient windows
       treemacs                             ; nvim-tree parity
       (vc-gutter +pretty)                  ; gitsigns parity (diff in fringe; uses diff-hl)
       workspaces                           ; project switching (neovim-project parity)

       :editor
       (evil +everywhere)                   ; vim keybindings (nvim parity)
       file-templates
       fold
       (format +onsave)                     ; conform.nvim parity
       multiple-cursors                     ; vim-visual-multi parity (evil-mc)
       snippets                             ; LuaSnip parity (yasnippet)

       :emacs
       dired                                ; oil.nvim rough parity
       electric
       undo                                 ; undotree parity (undo-tree)
       vc

       :term
       vterm                                ; embedded terminal (for claude-code.el)

       :checkers
       syntax                               ; flycheck diagnostics

       :tools
       (debugger +lsp)                      ; nvim-dap parity (dap-mode)
       direnv                               ; python venv auto-detect
       docker
       (eval +overlay)                      ; iron.nvim rough parity
       lookup                               ; nav to definitions/docs
       (lsp +peek)                          ; nvim-lspconfig parity
       magit                                ; neogit parity
       make                                 ; neotest rough parity
       tree-sitter
       (terraform)

       :os
       (:if IS-MAC macos)

       :lang
       emacs-lisp
       (go +lsp +tree-sitter)
       json
       (javascript +lsp +tree-sitter)
       (lua +lsp +tree-sitter)
       markdown
       (org
        +pretty
        +journal
        +present
        +hugo
        +lsp)
       (python +lsp +pyright +tree-sitter)
       (rust +lsp +tree-sitter)
       sh                                   ; sh module has no +tree-sitter flag
       solidity
       web
       yaml

       :config
       (default +bindings +smartparens))
