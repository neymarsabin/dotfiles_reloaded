return {
	"sindrets/diffview.nvim",
	dependencies = { "nvim-lua/plenary.nvim" },
	cmd = { "DiffviewOpen", "DiffviewFileHistory", "DiffviewClose" },
	keys = {
		{ "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diff working tree" },
		{ "<leader>gD", "<cmd>DiffviewClose<cr>", desc = "Close diffview" },
		{ "<leader>gh", "<cmd>DiffviewFileHistory<cr>", desc = "Repo file history" },
		{ "<leader>gH", "<cmd>DiffviewFileHistory %<cr>", desc = "Current file history" },
		{ "<leader>gh", "<esc><cmd>'<,'>DiffviewFileHistory<cr>", mode = "v", desc = "Selection history" },
		-- diff against a branch/commit: prompts for a ref
		{ "<leader>gm", function()
			local ref = vim.fn.input("Diff against ref: ", "origin/HEAD")
			if ref ~= "" then
				vim.cmd("DiffviewOpen " .. ref)
			end
		end, desc = "Diff against ref" },
	},
	opts = function()
		local actions = require("diffview.actions")
		return {
			enhanced_diff_hl = true,
			view = {
				default = { layout = "diff2_horizontal" },
				merge_tool = { layout = "diff3_mixed", disable_diagnostics = true },
			},
			keymaps = {
				view = {
					{ "n", "q", "<cmd>DiffviewClose<cr>", { desc = "Close" } },
					{ "n", "<tab>", actions.select_next_entry, { desc = "Next file" } },
					{ "n", "<s-tab>", actions.select_prev_entry, { desc = "Prev file" } },
				},
				file_panel = {
					{ "n", "q", "<cmd>DiffviewClose<cr>", { desc = "Close" } },
				},
				file_history_panel = {
					{ "n", "q", "<cmd>DiffviewClose<cr>", { desc = "Close" } },
				},
			},
		}
	end,
}
