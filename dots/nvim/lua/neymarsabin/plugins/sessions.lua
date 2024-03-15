return {
	"coffebar/neovim-project",
	config = function()
		-- enable saving the state of plugins in the session
		vim.opt.sessionoptions:append("globals") -- save global variables that start with an uppercase letter and contain at least one lowercase letter.
		-- setup neovim-project plugin
		require("neovim-project").setup({
			projects = { -- define project roots
				"~/projects/oss/*",
				"~/projects/work/*",
				"~/projects/pet/*",
				"~/projects/mine/*",
				"~/.config/nvim/",
			},
		})
		vim.keymap.set(
			"n",
			"<leader>pp",
			":Telescope neovim-project discover <CR>",
			{ desc = "Get all projects and enter to it" }
		)
	end,
	dependencies = {
		{ "nvim-lua/plenary.nvim" },
		{ "nvim-telescope/telescope.nvim", tag = "0.1.4" },
		{ "Shatur/neovim-session-manager" },
	},
}
