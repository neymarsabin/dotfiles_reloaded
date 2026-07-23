return {
	"nvim-neotest/neotest",
	dependencies = {
		"nvim-neotest/nvim-nio",
		"nvim-lua/plenary.nvim",
		"antoinemadec/FixCursorHold.nvim",
		"nvim-treesitter/nvim-treesitter",
		"nvim-neotest/neotest-python",
		"nvim-neotest/neotest-go",
	},
	keys = {
		{ "<leader>tn", function() require("neotest").run.run() end, desc = "Run nearest test" },
		{ "<leader>tf", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run file tests" },
		{ "<leader>ts", function() require("neotest").summary.toggle() end, desc = "Toggle test summary" },
		{ "<leader>to", function() require("neotest").output.open({ enter = true }) end, desc = "Show test output" },
		{ "<leader>tO", function() require("neotest").output_panel.toggle() end, desc = "Toggle output panel" },
		{ "<leader>tS", function() require("neotest").run.stop() end, desc = "Stop test" },
	},
	config = function()
		require("neotest").setup({
			adapters = {
				require("neotest-python")({
					dap = { justMyCode = false },
					runner = "pytest",
					python = ".venv/bin/python",
				}),
				require("neotest-go"),
			},
		})
	end,
}
