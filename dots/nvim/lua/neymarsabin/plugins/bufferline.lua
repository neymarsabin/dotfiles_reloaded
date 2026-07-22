return {
	"akinsho/bufferline.nvim",
	version = "*",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	event = { "BufReadPre", "BufNewFile" },
	config = function()
		require("bufferline").setup({
			options = {
				mode = "buffers",
				diagnostics = "nvim_lsp",
				diagnostics_indicator = function(count, level)
					local icon = level:match("error") and " " or " "
					return " " .. icon .. count
				end,
				separator_style = "slant",
				show_buffer_close_icons = true,
				show_close_icon = false,
				always_show_bufferline = true,
				offsets = {
					{ filetype = "neo-tree", text = "  Explorer", highlight = "Directory", separator = true },
				},
			},
		})

		local map = vim.keymap.set
		-- buffer navigation
		map("n", "<S-h>", "<cmd>BufferLineCyclePrev<cr>", { desc = "Prev buffer" })
		map("n", "<S-l>", "<cmd>BufferLineCycleNext<cr>", { desc = "Next buffer" })
		map("n", "<leader>bp", "<cmd>BufferLineTogglePin<cr>", { desc = "Pin buffer" })
		map("n", "<leader>bP", "<cmd>BufferLineGroupClose ungrouped<cr>", { desc = "Close unpinned" })
		map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete buffer" })
		map("n", "<leader>bo", "<cmd>BufferLineCloseOthers<cr>", { desc = "Close other buffers" })
		map("n", "<leader>bh", "<cmd>BufferLineCloseLeft<cr>", { desc = "Close buffers left" })
		map("n", "<leader>bl", "<cmd>BufferLineCloseRight<cr>", { desc = "Close buffers right" })
		-- jump to buffer 1-9 by ordinal position
		for i = 1, 9 do
			map("n", "<leader>" .. i, function()
				require("bufferline").go_to(i, true)
			end, { desc = "Go to buffer " .. i })
		end
	end,
}
