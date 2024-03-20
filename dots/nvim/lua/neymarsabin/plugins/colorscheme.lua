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
}
