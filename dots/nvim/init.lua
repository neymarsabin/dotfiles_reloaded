-- leader key must be set before lazy loads plugins
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- display
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.colorcolumn = "80"
vim.opt.wrap = true
vim.opt.linebreak = true
vim.opt.foldenable = false

-- use system clipboard for all yank/paste operations
vim.opt.clipboard = "unnamedplus"

-- enable 24-bit color (required for themes like catppuccin)
vim.opt.termguicolors = true

--- indentation ---
vim.opt.expandtab = true -- Use spaces instead of tabs
vim.opt.shiftwidth = 2 -- Indent size is 2 spaces
vim.opt.softtabstop = 2 -- Tab key inserts 2 spaces
vim.opt.tabstop = 2 -- Tab is displayed as 2 spaces
vim.opt.smartindent = true -- Smart indentation

-- vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
vim.opt.hlsearch = true
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- auto-reload buffers when files change on disk (e.g. Claude Code edits)
vim.opt.autoread = true
vim.opt.updatetime = 500
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" }, {
	pattern = "*",
	callback = function()
		if vim.fn.mode() ~= "c" then
			vim.cmd("checktime")
		end
	end,
})

-- exit modes with <C-g> ----
vim.keymap.set("n", "<leader>fs", "<cmd>w<CR>", { desc = "Save file...", noremap = true })
vim.keymap.set("i", "<C-g>", function()
	if vim.bo.filetype == "TelescopePrompt" then
		require("telescope.actions").close(vim.api.nvim_get_current_buf())
	else
		vim.cmd("stopinsert")
	end
end, { noremap = true, nowait = true })
vim.keymap.set("n", "<C-g>", "<ESC>", { noremap = true })
vim.keymap.set("v", "<C-g>", "<ESC>", { noremap = true })
vim.keymap.set("c", "<C-g>", "<ESC>", { noremap = true })
vim.keymap.set("t", "<C-g>", "<C-\\><C-n>", { noremap = true, desc = "Exit terminal mode" })

-- code diagnostics provided by nvim ---
vim.keymap.set("n", "<leader>d[", vim.diagnostic.goto_prev, { desc = "Go to previous [D]iagnostic message" })
vim.keymap.set("n", "<leader>d]", vim.diagnostic.goto_next, { desc = "Go to next [D]iagnostic message" })
vim.keymap.set("n", "<leader>de", vim.diagnostic.open_float, { desc = "Show diagnostic [E]rror messages" })
vim.keymap.set("n", "<leader>dq", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

-- split window view keybindings ---
vim.keymap.set("n", "<leader>wh", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<leader>wl", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<leader>wj", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<leader>wk", "<C-w><C-k>", { desc = "Move focus to the upper window" })
vim.keymap.set("n", "<leader>wn", "<cmd>vnew<CR>", { desc = "Create a new window horizontally " })
vim.keymap.set("n", "<leader>wv", "<cmd>vsplit<CR>", { desc = "Create a new window horizontally " })
vim.keymap.set("n", "<leader>wq", "<cmd>close<CR>", { desc = "Close the window " })

-- move line up or down
vim.keymap.set("v", "<C-j>", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "<C-k>", ":m '<-2<CR>gv=gv")

-- navigate page and center focus
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- send text to the claude code pane in the same tmux window
vim.keymap.set("v", "<leader>tp", function()
	require("neymarsabin.claude_tmux").send_selection()
end, { desc = "Send selection to Claude pane" })
vim.keymap.set("n", "<leader>tp", function()
	require("neymarsabin.claude_tmux").send_clipboard()
end, { desc = "Send clipboard to Claude pane" })

-- inline diff overlay, opened by claude's Stop hook when it changed code
require("neymarsabin.claude_diff").listen()
vim.keymap.set("n", "<leader>hv", function()
	require("neymarsabin.claude_diff").toggle()
end, { desc = "Toggle Claude inline diff" })

-- install lazy vim plugins manager ---
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

-- setup lazyvim and package management ---
-- installed packages ----
require("lazy").setup({
	{ import = "neymarsabin.plugins" },
})
