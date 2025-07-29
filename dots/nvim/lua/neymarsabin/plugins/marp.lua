return {
	"mpas/marp-nvim",
	config = function()
		require("marp").setup({
			port = 8898,
			wait_for_response_timeout = 30,
			wait_for_response_delay = 1,
		})
		vim.keymap.set("n", "<leader>MT", "<cmd>MarpToggle<cr>", { noremap = true, silent = true })
		vim.keymap.set("n", "<leader>MS", "<cmd>MarpStatus<cr>", { noremap = true, silent = true })
	end,
}
