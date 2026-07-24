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
		local map = vim.keymap.set
		map("n", "<leader>gg", function() neogit.open() end, { desc = "Neogit status" })
		map("n", "<leader>gc", function() neogit.open({ "commit" }) end, { desc = "Git commit" })
		map("n", "<leader>gP", function() neogit.open({ "push" }) end, { desc = "Git push" })
		map("n", "<leader>gp", function() neogit.open({ "pull" }) end, { desc = "Git pull" })
		map("n", "<leader>gb", "<cmd>Telescope git_branches<cr>", { desc = "Git branches" })
		map("n", "<leader>gl", "<cmd>Telescope git_commits<cr>", { desc = "Git log (commits)" })
		map("n", "<leader>gf", "<cmd>Telescope git_bcommits<cr>", { desc = "File commits" })
		map("n", "<leader>gs", "<cmd>Telescope git_status<cr>", { desc = "Git status (telescope)" })
	end,
}
