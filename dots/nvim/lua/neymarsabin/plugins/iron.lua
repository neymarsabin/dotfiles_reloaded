return {
	"Vigemus/iron.nvim",
	keys = {
		{ "<leader>io", "<cmd>IronRepl<cr>", desc = "Open REPL" },
		{ "<leader>ir", "<cmd>IronRestart<cr>", desc = "Restart REPL" },
		{ "<leader>ix", "<cmd>IronHide<cr>", desc = "Hide REPL" },
		{ "<leader>il", function() require("iron.core").send_line() end, desc = "Send line to REPL" },
		{ "<leader>is", function() require("iron.core").visual_send() end, mode = "v", desc = "Send selection to REPL" },
		{ "<leader>if", function() require("iron.core").send_file() end, desc = "Send file to REPL" },
	},
	config = function()
		require("iron.core").setup({
			config = {
				repl_definition = {
					python = {
						command = function()
							-- prefer ipython if available
							if vim.fn.executable("ipython") == 1 then
								return { "ipython", "--no-autoindent" }
							end
							return { "python3" }
						end,
					},
					go = { command = { "gore" } },
					lua = { command = { "lua" } },
				},
				repl_open_cmd = "vertical botright 80 split",
			},
			keymaps = {
				send_motion = "<leader>ic",
				mark_motion = "<leader>im",
				mark_visual = "<leader>im",
				remove_mark = "<leader>iD",
				cr = "<leader>i<cr>",
				interrupt = "<leader>i,",
				exit = "<leader>iq",
				clear = "<leader>iL",
			},
		})
	end,
}
