return {
	"folke/trouble.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	config = function()
		require("trouble").setup({
			position = "bottom",
			height = 10,
			width = 50,
			icons = true,
			group = true,
			padding = true,
			cycle_list = true,
			mode = "workspace_diagnostics",
			action_keys = {
				close = "q",
				cancel = "<esc>",
				refresh = "r",
				jump = { "<cr>", "<tab>" },
				jump_close = { "o" },
				toggle_mode = "m",
				toggle_preview = "P",
				toggle_group = "g",
				next = "j",
				previous = "k",
				hover = "K",
			},
			signs = {
				-- icons / text used for a diagnostic
				error = "",
				warning = "",
				hint = "",
				information = "",
				other = "",
			},
			vim.keymap.set("n", "<leader>tt", "<cmd>Trouble<cr>", {}),
		})
	end,
	opts = {},
}
