return {
	"folke/noice.nvim",
	event = "VeryLazy",
	dependencies = {
		"MunifTanjim/nui.nvim",
		{
			"rcarriga/nvim-notify",
			opts = {
				timeout = 3000, -- auto-dismiss after 3s
				stages = "fade_in_slide_out",
				render = "minimal", -- clean, no heavy borders/icons
				background_colour = "#1e1e2e", -- avoid transparent black box
				fps = 60,
				top_down = false, -- newest at bottom-right
				max_width = 60,
			},
		},
	},
	opts = {
		cmdline = {
			view = "cmdline", -- bottom row (classic), not centered popup
		},
		lsp = {
			-- route LSP markdown through treesitter for nice hover/signature
			override = {
				["vim.lsp.util.convert_input_to_markdown_lines"] = true,
				["vim.lsp.util.stylize_markdown"] = true,
				["cmp.entry.get_documentation"] = true,
			},
			signature = { enabled = true },
			hover = { enabled = true },
			progress = { enabled = false }, -- fidget.nvim already shows LSP progress (lsp.lua)
		},
		presets = {
			bottom_search = true, -- classic bottom / for search
			long_message_to_split = true, -- long messages go to a split
			lsp_doc_border = true, -- add a border to hover docs and signature help
		},
		routes = {
			-- silence "written" file save spam
			{
				filter = { event = "msg_show", kind = "", find = "written" },
				opts = { skip = true },
			},
		},
	},
	config = function(_, opts)
		require("noice").setup(opts)
		vim.notify = require("notify")

		local map = vim.keymap.set
		map("n", "<leader>nl", function()
			require("noice").cmd("last")
		end, { desc = "Noice last message" })
		map("n", "<leader>nh", function()
			require("noice").cmd("history")
		end, { desc = "Noice history" })
		map("n", "<leader>nd", function()
			require("noice").cmd("dismiss")
		end, { desc = "Dismiss notifications" })
	end,
}
