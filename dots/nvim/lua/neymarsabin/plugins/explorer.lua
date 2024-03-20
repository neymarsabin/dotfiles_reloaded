return {
	{
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
	},
	{
		"stevearc/oil.nvim",
		config = function()
			require("oil").setup({
				keymaps = {
					["g?"] = "actions.show_help",
					["<CR>"] = "actions.select",
					["<C-s>"] = "actions.select_vsplit",
					["<C-h>"] = "actions.select_split",
					["<C-t>"] = "actions.select_tab",
					["<C-p>"] = "actions.preview",
					["<C-c>"] = "actions.close",
					["<C-l>"] = "actions.refresh",
					["-"] = "actions.parent",
					["_"] = "actions.open_cwd",
					["`"] = "actions.cd",
					["~"] = "actions.tcd",
					["gs"] = "actions.change_sort",
					["gx"] = "actions.open_external",
					["g."] = "actions.toggle_hidden",
					["g\\"] = "actions.toggle_trash",
				},
				vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" }),
			})
		end,
	},
}
