;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

;; ============================================================================
;; Existing packages (preserved)
;; ============================================================================
(package! lsp-mode)
(package! chatgpt-shell)
(package! zenburn-theme)
(package! elfeed)
(package! elfeed-dashboard)
(package! jazz-theme)
(package! xclip)
(package! kanagawa-themes)  ;; MELPA recipe is plural; old declaration was a typo
(package! go-playground)

;; GitHub Copilot (inline completion)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

;; ============================================================================
;; Neovim-parity additions
;; ============================================================================

;; Catppuccin Mocha — matches Neovim colorscheme
(package! catppuccin-theme)

;; Claude Code integration — parity with claudecode.nvim
;; Runs the `claude` CLI in a side panel; supports toggle/resume/continue/send-region.
(package! claude-code
  :recipe (:host github :repo "stevemolitor/claude-code.el"
           :files ("*.el")))

;; Multi-provider AI chat with streaming + markdown — parity with avante.nvim
(package! gptel)

;; Modern Dired replacement — parity with oil.nvim (edit directories as buffers)
(package! dirvish)

;; Harpoon — quick file marks, parity with ThePrimeagen/harpoon
(package! harpoon
  :recipe (:host github :repo "otavioschwanck/harpoon.el"))

;; Project-wide find & replace — parity with nvim-spectre
(package! deadgrep)

;; REPL send-to — parity with iron.nvim
;; (doom's `:tools eval +overlay` covers elisp; this adds python/other REPL support)
(package! code-cells)

