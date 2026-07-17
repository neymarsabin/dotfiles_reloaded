;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "neymarsabin"
      user-mail-address "reddevil.sabin@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Monaco" :size 12 :weight 'normal)
;;       doom-variable-pitch-font (font-spec :family "Monaco" :size 12))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; Theme — Catppuccin Mocha (matches Neovim colorscheme).
;; Must require the package so `load-theme 'catppuccin` finds it.
;; Latte is the light variant: (setq catppuccin-flavor 'latte) + M-x doom/reload-theme.
(use-package! catppuccin-theme
  :demand t
  :init (setq catppuccin-flavor 'mocha))
(setq doom-theme 'catppuccin)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/mine/myself/the-new-org/doom/capture")
(setq org-journal-dir "~/projects/mine/myself/the-new-org/doom/capture")
(setq org-hugo-base-dir "~/projects/mine/portfolio")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; enable LSP for Javascript
;; (after! lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection "typescript-language-server --stdio")
;;                     :major-modes '(js2-mode typescript-mode web-mode typescript-tsx-mode)
;;                     :server-id 'ts-ls)))

;; set org agenda files
(setq org-agenda-files (list "~/projects/mine/myself/the-new-org/doom/capture"))

;; set org agenda list
(setq org-agenda-span 7)

;; lsp mode setup for golang
;; (add-hook 'go-mode-hook #'lsp-deferred)
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; disable title bar
(menu-bar-mode -1)
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; duplicate current line
(defun duplicate-line ()
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (insert (thing-at-point 'line t))))

(global-set-key (kbd "C-S-d") 'duplicate-line)

;; move a line up or down
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "M-k") 'move-line-up)
(global-set-key (kbd "M-j") 'move-line-down)

;; maximize the window upon startup
(setq initial-frame-alist '((top . 1) (left . 1) (width . 114) (height . 32)))

;; enable variable and visual line mode in Org mode by default
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

;; org-capture configurations
(defun zz/add-file-keybinding (key file &optional desc)
  (let ((key key)
        (file file)
        (desc desc))
    (map! :desc (or desc file)
          key
          (lambda () (interactive) (find-file file)))))

(zz/add-file-keybinding "C-c z i" "~/projects/mine/myself/the-new-org/doom/capture/ideas.org" "ideas.org")

;; disable global highlight line mode, irritating when using visual mode
;; TODO: maybe best to uninstall the whole package hl-line-mode
;; also for some reason, the config below does not work in Elisp files
(setq hl-line-mode nil)

;; wrap a word or region with something
;; TODO: find some shortcuts to use these
(defun insert-curves (&optional arg)
  "Inserts {  } curves to the selected region."
  (interactive "P")
  (insert-pair arg ?\{ ?\}))
(global-set-key (kbd "M-S-{") 'insert-curves)

;; (defun insert-bigboots (&optional arg)
;;   "Inserts [ ] curves to the selected region.
;; Same here :D I wish I could say I wrote this function, copies everything of the function insert-parenthesis.
;; Rely on your LSP for indentation, couldn't write a single thing on indenting."
;;   (interactive "P")
;;   (insert-pair arg ?\[ ?\]))
;; ;; (global-set-key (kbd "M-S-^") 'insert-bigboots)

;; ChatGPT Configuration
(defun get-openai-key-from-env ()
  (getenv "OPENAI_API_KEY"))
(setq chatgpt-shell-openai-key (get-openai-key-from-env))

;; Elfeed RSS list
(setq elfeed-feeds
      '("https://alexwlchan.net/atom.xml"
        "https://news.ycombinator.com/rss"
        "http://feeds.feedburner.com/AlexSexton"))

;; open my .zshrc file when I press these key strokes
;; use evil-define-key from evil mode || docs link: https://evil.readthedocs.io/en/latest/keymaps.html#leader-keys
(defun neymar/open-zshrc ()
  (interactive)
  (find-file "~/.zshrc")
  (message "echo:: zshrc"))
(evil-define-key 'normal 'global (kbd "SPC fz") 'neymar/open-zshrc)

;; terraform lsp has issues with emacs30, does not allow other lsp's to work
;; found this fix from the issue mentioned below
(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages))

;; change cursor type in insert mode in terminal
(add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
(add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\033[2 q")))

;; add new custom org-capture templates for readme
(setq org-capture-templates
      '(("r" "Readme" entry (file+headline "~/.org/notes.org" "Readme")
         "* TODO %?\n  %i\n DEADLINE: %t \n - [ ] url: \n %a")))

;; xclip will allow us to copy paste from emacs to outside
(xclip-mode 1)

;; copilot configuration — only activate when the language server exists.
;; Without this guard, copilot-mode throws on prog-mode-hook and cascades,
;; killing font-lock, LSP, and company activation for every code buffer.
;; To enable: upgrade Node ≥ 20, then `M-x copilot-install-server`.
(defun +my/copilot-server-available-p ()
  "Return non-nil when the Copilot language server is installed."
  (or (executable-find "copilot-language-server")
      (and (boundp 'copilot-install-dir)
           copilot-install-dir
           (file-exists-p copilot-install-dir))
      (file-exists-p (expand-file-name "copilot/dist/language-server.js"
                                       (or (bound-and-true-p doom-cache-dir)
                                           user-emacs-directory)))))

(use-package! copilot
  :defer t
  :init
  (add-hook! 'prog-mode-hook
    (defun +my/copilot-maybe-enable-h ()
      (when (+my/copilot-server-available-p)
        (copilot-mode +1))))
  :bind (:map copilot-completion-map
              ("<tab>"   . #'copilot-accept-completion-by-paragraph)
              ("TAB"     . #'copilot-accept-completion-by-paragraph)
              ("C-TAB"   . #'copilot-accept-completion-by-paragraph)
              ("C-<tab>" . #'copilot-accept-completion-by-paragraph)))

;; set the default browser to firefox
(setq browse-url-browser-function 'browse-url-firefox)

;; configuration to display relative line numbers
(setq display-line-numbers-type 'relative)

;; wrap a word or region with something
(defun neymar/wrap-word (char)
  "Inserts the character at the front and end of the selected word.
   Very useful in org mode files to wrap a word with * or ~ or +"
  (interactive "c")
  (let ((word (buffer-substring-no-properties (mark) (point))))
    (delete-region (region-beginning) (region-end))
    (insert (concat (char-to-string char) word (char-to-string char)))))

;; bind the function to a key
(global-set-key (kbd "C-c w") 'neymar/wrap-word)


;; ============================================================================
;; NEOVIM PARITY LAYER
;; Mirrors keybinds/theme/AI-integration from ~/.config/nvim/
;; ============================================================================

;; ---- Ensure ~/.local/bin is on PATH so claude-code.el finds the `claude` CLI
(let ((local-bin (expand-file-name "~/.local/bin")))
  (when (file-directory-p local-bin)
    (add-to-list 'exec-path local-bin)
    (setenv "PATH" (concat local-bin ":" (getenv "PATH")))))

;; ---- Tree-sitter (Emacs 30 native treesit) ---------------------------------
;; Doom's `+tree-sitter` flag per-lang module handles `set-tree-sitter!` and
;; `major-mode-remap-alist` already (see modules/lang/*/config.el). We only
;; need to bump font-lock detail and declare extra grammar sources for
;; languages not covered by Doom out-of-the-box, so `M-x treesit-install-
;; language-grammar` works for all of them.
;; Python grammar is auto-pinned by Doom to v0.23.6 on Emacs 30 (ABI v14).
;; Ref: https://github.com/doomemacs/doomemacs/issues/8503
(setq treesit-font-lock-level 4)  ; max decoration (decorators, dunders, etc)

(after! treesit
  (dolist (src '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
                 (json       "https://github.com/tree-sitter/tree-sitter-json")
                 (yaml       "https://github.com/ikatyang/tree-sitter-yaml")
                 (toml       "https://github.com/tree-sitter/tree-sitter-toml")
                 (html       "https://github.com/tree-sitter/tree-sitter-html")
                 (css        "https://github.com/tree-sitter/tree-sitter-css")
                 (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                 (markdown   "https://github.com/ikatyang/tree-sitter-markdown")))
    (add-to-list 'treesit-language-source-alist src)))

;; Helper: install all declared grammars in one shot.
;; Usage:  M-x my/install-all-treesit-grammars
(defun my/install-all-treesit-grammars ()
  "Install every grammar listed in `treesit-language-source-alist'."
  (interactive)
  (dolist (entry treesit-language-source-alist)
    (let ((lang (car entry)))
      (unless (treesit-language-available-p lang)
        (message "Installing tree-sitter grammar: %s" lang)
        (condition-case err
            (treesit-install-language-grammar lang)
          (error (message "  ✗ %s: %s" lang (error-message-string err))))))))

;; ---- Which-key group descriptions — matches Neovim groups where possible.
;; SPC m stays as Doom's localleader (mode-specific); harpoon moves to SPC j.
(after! which-key
  (which-key-add-key-based-replacements
    "SPC a"  "ai/claude"
    "SPC j"  "jump/harpoon"))

;; ---- Claude Code — parity with claudecode.nvim
(use-package! claude-code
  :defer t
  :config
  (setq claude-code-terminal-backend 'vterm)
  (claude-code-mode +1))

;; ---- gptel — multi-provider AI chat, parity with avante.nvim
(use-package! gptel
  :defer t
  :config
  (setq gptel-default-mode 'markdown-mode))

;; All SPC a (ai/claude) bindings in ONE prefix block — mixing prefix and
;; non-prefix forms on the same leader key crashes map!.
(map! :leader
      (:prefix-map ("a" . "ai/claude")
       ;; Claude Code
       :desc "Toggle Claude Code"    "c" #'claude-code-toggle
       :desc "Start (or switch)"     "S" #'claude-code
       :desc "Send region"           "s" #'claude-code-send-region
       :desc "Add current buffer"    "b" #'claude-code-send-buffer-file
       :desc "Fix error at point"    "e" #'claude-code-fix-error-at-point
       :desc "Continue (/continue)"  "C" #'claude-code-continue
       :desc "Resume session"        "r" #'claude-code-resume
       :desc "Cycle edit mode"       "m" #'claude-code-cycle-mode
       :desc "Select Claude buffer"  "B" #'claude-code-select-buffer
       :desc "Transient menu"        "t" #'claude-code-transient
       ;; gptel (avante parity)
       :desc "gptel chat buffer"     "G" #'gptel
       :desc "gptel send"            "g" #'gptel-send
       :desc "gptel menu"            "M" #'gptel-menu))

;; ---- Harpoon — parity with ThePrimeagen/harpoon
(use-package! harpoon
  :defer t)

;; Harpoon under SPC j (jump) — SPC m is Doom's localleader, can't override.
(map! :leader
      (:prefix-map ("j" . "jump/harpoon")
       :desc "Add file"         "a" #'harpoon-add-file
       :desc "Quick menu"       "m" #'harpoon-quick-menu-hydra
       :desc "Slot 1"           "s" #'harpoon-go-to-1
       :desc "Slot 2"           "d" #'harpoon-go-to-2
       :desc "Slot 3"           "f" #'harpoon-go-to-3
       :desc "Slot 4"           "g" #'harpoon-go-to-4
       :desc "Slot 5"           "h" #'harpoon-go-to-5))

;; ---- File tree: treemacs — parity with nvim-tree (SPC e toggle)
(map! :leader :desc "Toggle file tree" "e" #'+treemacs/toggle)

;; ---- Dirvish — parity with oil.nvim (open parent dir buffer via `-`)
(use-package! dirvish
  :init (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons file-time file-size)))

(map! :nv "-" #'dirvish-dwim)

;; ---- Project / file search — parity with telescope
(map! :leader
      :desc "Find files in project" "p f" #'projectile-find-file
      :desc "Grep in project"       "p /" #'+default/search-project
      :desc "Git-tracked files"     "p g" #'projectile-find-file-in-known-projects
      :desc "Switch project"        "p p" #'projectile-switch-project
      :desc "Resume last search"    "s r" #'vertico-repeat
      :desc "Buffers"               "b b" #'consult-buffer
      :desc "Commands"              "c c" #'execute-extended-command
      :desc "Buffer line search"    "s s" #'consult-line
      :desc "Grep open buffers"     "s /" #'consult-line-multi
      :desc "Symbol outline"        "s o" #'consult-imenu
      :desc "Find in Doom config"   "s n" (lambda ()
                                            (interactive)
                                            (doom-project-find-file doom-user-dir))
      :desc "Project find/replace"  "s R" #'deadgrep
      :desc "Help (function)"       "h f" #'describe-function
      :desc "Keymaps"               "h k" #'describe-keymap)

;; ---- Diagnostics cycling — parity with Neovim ]d / [d + SPC d [ / SPC d ]
(map! :n "]d" #'flycheck-next-error
      :n "[d" #'flycheck-previous-error
      :leader
      :desc "Next diagnostic"   "d ]" #'flycheck-next-error
      :desc "Prev diagnostic"   "d [" #'flycheck-previous-error
      :desc "List diagnostics"  "d l" #'flycheck-list-errors)

;; ---- Window — Doom's default SPC w already maps to evil-window-map:
;; SPC w v/s/c/h/j/k/l work out of the box. No custom bindings needed.

;; ---- File group — SPC f s = save
(map! :leader :desc "Save file" "f s" #'save-buffer)

;; ---- Auto-reload buffers when changed on disk (Claude Code edits externally)
(global-auto-revert-mode 1)
(setq auto-revert-use-notify t
      auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

;; ---- Center scroll on C-d / C-u (nvim parity)
(map! :nv "C-d" (lambda () (interactive) (evil-scroll-down nil) (evil-scroll-line-to-center nil))
      :nv "C-u" (lambda () (interactive) (evil-scroll-up nil)   (evil-scroll-line-to-center nil)))

;; ---- Clear highlight on Esc (nvim parity)
(map! :n "<escape>" #'evil-ex-nohighlight)

;; ---- Move selected lines up/down with Ctrl-j / Ctrl-k in visual mode
(map! :v "C-j" #'drag-stuff-down
      :v "C-k" #'drag-stuff-up)

;; ---- Exit insert/terminal mode with C-g (already default in Emacs, confirmed here)
;; C-g is already bound globally; no override needed.

