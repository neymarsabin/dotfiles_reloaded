return {
	{
		"rose-pine/neovim",
		name = "rose-pine",
		config = function()
			require("rose-pine").setup({
				styles = {
					bold = true,
					italic = false,
					transparency = false,
				},
				enable = {
					terminal = true,
					legacy_highlights = true,
					migrations = true,
				},
			})
			vim.cmd.colorscheme("rose-pine")
		end,
		opts = {},
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = { signs = false },
	},
}
