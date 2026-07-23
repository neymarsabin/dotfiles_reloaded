return {
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"rcarriga/nvim-dap-ui",
			"nvim-neotest/nvim-nio",
			"mfussenegger/nvim-dap-python",
			"leoluz/nvim-dap-go",
			"theHamsta/nvim-dap-virtual-text",
		},
		keys = {
			{ "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle breakpoint" },
			{ "<leader>dB", function() require("dap").set_breakpoint(vim.fn.input("Condition: ")) end, desc = "Conditional breakpoint" },
			{ "<leader>dc", function() require("dap").continue() end, desc = "Continue / Start" },
			{ "<leader>dn", function() require("dap").step_over() end, desc = "Step over (next)" },
			{ "<leader>di", function() require("dap").step_into() end, desc = "Step into" },
			{ "<leader>do", function() require("dap").step_out() end, desc = "Step out" },
			{ "<leader>dr", function() require("dap").restart() end, desc = "Restart" },
			{ "<leader>dx", function() require("dap").terminate() end, desc = "Terminate" },
			{ "<leader>du", function() require("dapui").toggle() end, desc = "Toggle DAP UI" },
			{ "<leader>dk", function() require("dap.ui.widgets").hover() end, desc = "Inspect variable" },
			{ "<leader>dl", function() require("dap").run_last() end, desc = "Run last config" },
		},
		config = function()
			local dap = require("dap")
			local dapui = require("dapui")

			-- DAP UI setup
			dapui.setup({
				layouts = {
					{
						elements = {
							{ id = "scopes", size = 0.4 },
							{ id = "breakpoints", size = 0.2 },
							{ id = "stacks", size = 0.2 },
							{ id = "watches", size = 0.2 },
						},
						position = "left",
						size = 40,
					},
					{
						elements = {
							{ id = "repl", size = 0.5 },
							{ id = "console", size = 0.5 },
						},
						position = "bottom",
						size = 10,
					},
				},
			})

			-- auto open/close UI on debug session
			dap.listeners.after.event_initialized["dapui_config"] = function()
				dapui.open()
			end
			dap.listeners.before.event_terminated["dapui_config"] = function()
				dapui.close()
			end
			dap.listeners.before.event_exited["dapui_config"] = function()
				dapui.close()
			end

			-- virtual text (show variable values inline)
			require("nvim-dap-virtual-text").setup()

			-- Python DAP
			local python_path = function()
				local cwd = vim.fn.getcwd()
				for _, name in ipairs({ ".venv", "venv" }) do
					local path = cwd .. "/" .. name .. "/bin/python"
					if vim.fn.executable(path) == 1 then
						return path
					end
				end
				local venv = os.getenv("VIRTUAL_ENV")
				if venv then
					return venv .. "/bin/python"
				end
				return "python3"
			end
			require("dap-python").setup(python_path())

			-- Go DAP
			require("dap-go").setup()
		end,
	},
}
