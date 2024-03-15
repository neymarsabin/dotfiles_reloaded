--- vim related configurations --[[ ---- ]]
vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.foldmethod = "indent"
vim.opt.foldenable = false
vim.opt.clipboard = "unnamedplus"

-- vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
vim.opt.hlsearch = true
-- vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- exit modes with <C-g> ----
vim.keymap.set("n", "<leader>fs", "<cmd>w<CR>", { desc = "Save file...", noremap = true })
vim.keymap.set("i", "<C-g>", "<ESC>", { noremap = true })
vim.keymap.set("n", "<C-g>", "<ESC>", { noremap = true })
vim.keymap.set("v", "<C-g>", "<ESC>", { noremap = true })
vim.keymap.set("c", "<C-g>", "<ESC>", { noremap = true })

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
vim.keymap.set("n", "<C-k>", "<cmd>m .-2<cr>==", { desc = "move line up", noremap = true })
vim.keymap.set("n", "<C-j>", "<cmd>m .+1<cr>==", { desc = "move line down", noremap = true })

-- -- move visual block up or down
-- vim.keymap.set("v", "<A-j>", "<cmd>m '>+1<cr>gv=gv", { desc = "move visual block down", noremap = true })
-- vim.keymap.set("v", "<A-k>", "<cmd>m '<-2<cr>gv=gv", { desc = "move visual block up", noremap = true })

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
