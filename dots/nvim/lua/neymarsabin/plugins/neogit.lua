return {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim",
		"sindrets/diffview.nvim",
		"nvim-telescope/telescope.nvim",
	},
	config = function()
		local neogit = require("neogit")
		neogit.setup({
			disable_context_highlighting = false,
			signs = {
				-- { CLOSED, OPENED }
				section = { "", "" },
				item = { "", "" },
				hunk = { "", "" },
			},
			mappings = {
				status = {
					["q"] = "Close",
				},
			},
		})
		vim.keymap.set("n", "<leader>gg", ":Neogit <CR>", { desc = "NeoGit Status" })
	end,
}
