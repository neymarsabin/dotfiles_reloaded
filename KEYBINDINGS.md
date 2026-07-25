# Neovim Keybindings Reference

Leader key: `<Space>`

## General

| Key | Action |
|---|---|
| `<Esc>` | Clear search highlight |
| `<leader>fs` | Save file |
| `<C-g>` | Exit any mode (insert/visual/command/terminal) |
| `<C-d>` / `<C-u>` | Page down/up (centered) |

## Window Management

| Key | Action |
|---|---|
| `<leader>wh/wl/wj/wk` | Move focus left/right/down/up |
| `<leader>wn` | New horizontal window |
| `<leader>wv` | Vertical split |
| `<leader>wq` | Close window |

## Navigation (Flash)

| Key | Action |
|---|---|
| `s` + type chars | Jump anywhere on screen |
| `S` | Flash treesitter select |

## Search & Replace

| Key | Action |
|---|---|
| `<leader>pf` | Find files |
| `<leader>pg` | Git files |
| `<leader>p/` | Live grep |
| `<leader>ss` | Fuzzy search in buffer |
| `<leader>s/` | Grep in open files |
| `<leader>sr` | Resume last telescope |
| `<leader>sR` | Spectre find & replace |
| `<leader>sw` | Search word under cursor |
| `<leader>sp` | Search & replace in current file |
| `<leader>sn` | Search neovim config |
| `<leader>st` | Search TODOs |
| `<leader>se` | Search emoji |
| `<leader>bb` | Buffers |
| `<leader>cc` | Commands |
| `<leader>hf` | Help tags |
| `<leader>hk` | Keymaps |

## LSP / Code

| Key | Action |
|---|---|
| `gd` | Go to definition |
| `gr` | Go to references |
| `gi` | Go to implementation |
| `gD` | Go to declaration |
| `K` | Hover docs / type info |
| `<leader>pd` | Peek definition |
| `<C-s>` | Signature help |
| `<leader>rn` | Rename symbol |
| `<leader>ca` | Code action |
| `<leader>D` | Type definition |
| `<leader>ds` | Document symbols |
| `<leader>ws` | Workspace symbols |
| `<leader>so` | Toggle symbol outline |

## Diagnostics

| Key | Action |
|---|---|
| `<leader>ge` | Show error popup |
| `<leader>d[` / `<leader>d]` | Prev/next diagnostic |
| `<leader>dq` | Diagnostics to quickfix |
| `<leader>tt` | Toggle trouble panel |
| `]t` / `[t` | Next/prev TODO |

## Debugging (DAP)

| Key | Action |
|---|---|
| `<leader>db` | Toggle breakpoint |
| `<leader>dB` | Conditional breakpoint |
| `<leader>dc` | Start / Continue |
| `<leader>dn` | Step over |
| `<leader>di` | Step into |
| `<leader>do` | Step out |
| `<leader>dr` | Restart |
| `<leader>dx` | Terminate |
| `<leader>du` | Toggle DAP UI |
| `<leader>dk` | Inspect variable |
| `<leader>dl` | Run last config |

## Testing

| Key | Action |
|---|---|
| `<leader>tn` | Run nearest test |
| `<leader>tf` | Run file tests |
| `<leader>ts` | Toggle test summary |
| `<leader>to` | Show test output |
| `<leader>tO` | Toggle output panel |
| `<leader>tS` | Stop test |

## REPL

| Key | Action |
|---|---|
| `<leader>io` | Open REPL |
| `<leader>il` | Send line |
| `<leader>is` | Send selection (visual) |
| `<leader>if` | Send file |
| `<leader>ir` | Restart REPL |
| `<leader>iq` | Exit REPL |

## Git

| Key | Action |
|---|---|
| `<leader>gg` | Neogit status |
| `]h` / `[h` | Next/prev hunk |
| `<leader>hs` | Stage hunk |
| `<leader>hr` | Reset hunk |
| `<leader>hS` / `<leader>hR` | Stage/reset buffer |
| `<leader>hu` | Undo stage |
| `<leader>hp` / `<leader>hP` | Preview hunk inline/float |
| `<leader>hb` | Blame line |
| `<leader>hd` | Diff this |

## Text Objects (visual/operator)

| Key | Action |
|---|---|
| `af` / `if` | Around/inside function |
| `ac` / `ic` | Around/inside class |
| `aa` / `ia` | Around/inside argument |
| `ai` / `ii` | Around/inside if |
| `al` / `il` | Around/inside loop |
| `]m` / `[m` | Next/prev method |
| `]c` / `[c` | Next/prev class |
| `<CR>` / `<BS>` | Expand/shrink treesitter selection |
| `<leader>sa` / `<leader>sA` | Swap arg next/prev |

## Surround

| Key | Action |
|---|---|
| `ys{motion}{char}` | Add surround (e.g. `ysiw"`) |
| `cs{old}{new}` | Change surround (e.g. `cs"'`) |
| `ds{char}` | Delete surround |
| `S{char}` | Surround visual selection |

## Multi-cursor

| Key | Action |
|---|---|
| `<C-n>` | Select word, repeat for next |
| `<C-Up>` / `<C-Down>` | Add cursor above/below |
| `q` / `Q` | Skip / remove region |
| `<C-S-n>` | Select all occurrences |

## Claude Code

| Key | Action |
|---|---|
| `<leader>ac` | Toggle Claude |
| `<leader>af` | Focus Claude |
| `<leader>ar` | Resume session |
| `<leader>aC` | Continue session |
| `<leader>as` | Send selection (visual) |
| `<leader>ab` | Add current buffer |
| `<leader>aa` / `<leader>ad` | Accept/reject diff |
| `<leader>tp` | Yank to Claude tmux pane (visual) |

## Files & Projects

| Key | Action |
|---|---|
| `<leader>e` | Toggle file tree |
| `-` | Oil file browser |
| `<leader>rr` | Ranger |
| `<leader>pp` | Discover projects |
| `<leader>u` | Undo tree |
| `<leader>ma` | Add to harpoon |
| `<leader>ms/md/mf/mg/mh` | Jump to mark 1-5 |
| `<leader>mm` | Search harpoon marks |

## Completion

| Key | Action |
|---|---|
| `<C-n>` / `<C-p>` | Next/prev completion |
| `<TAB>` | Accept completion |
| `<C-Space>` | Trigger completion |
| `<C-l>` / `<C-h>` | Jump in snippet |

## Comments

| Key | Action |
|---|---|
| `gcc` / `gbc` | Toggle line/block comment |
| `gcO` / `gco` / `gcA` | Comment above/below/end of line |
