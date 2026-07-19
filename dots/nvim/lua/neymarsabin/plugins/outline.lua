return {
	"hedyhli/outline.nvim",
	cmd = { "Outline", "OutlineOpen" },
	keys = {
		{ "<leader>so", "<cmd>Outline<cr>", desc = "Toggle symbol outline" },
	},
	opts = {
		outline_window = {
			position = "right",
			width = 25,
		},
		symbols = {
			filter = nil,
		},
		symbol_folding = {
			autofold_depth = 1,
		},
	},
}
