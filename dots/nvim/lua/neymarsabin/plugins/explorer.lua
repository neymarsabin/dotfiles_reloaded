return {
	"nvim-tree/nvim-tree.lua",
	version = "*",
	lazy = false,
	dependencies = {
		"nvim-tree/nvim-web-devicons",
	},
	config = function()
		local nvimtree = require("nvim-tree")
		local filePath = vim.fn.getcwd()
		print("Current working directory: " .. filePath)
		nvimtree.setup({
			sort = {
				sorter = "case_sensitive",
			},
			filters = {
				dotfiles = true,
			},
		})
		vim.keymap.set("n", "<leader>e", ":NvimTreeToggle<CR>", {
			silent = true,
			noremap = true,
			desc = "Toggle [E]xplorer",
		})
	end,
}
