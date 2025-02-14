return {
	"nvim-treesitter/nvim-treesitter",
	build = ":TSUpdate",
	config = function()
		require("nvim-treesitter.configs").setup({
			modules = {},
			ensure_installed = {
				"bash",
				"html",
				"lua",
				"go",
				"toml",
				"yaml",
				"json",
				"javascript",
				"typescript",
				"css",
				"scss",
				"tsx",
				"markdown",
				"terraform",
			},
			auto_install = true,
			sync_install = false,
			highlight = { enable = true },
			indent = { enable = true },
			ignore_install = {},
		})
	end,
}
