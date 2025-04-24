return {
	"allaman/emoji.nvim",
	version = "1.0.0", -- optionally pin to a tag
	ft = "markdown", -- adjust to your needs
	dependencies = {
		-- optional for telescope integration
		"nvim-telescope/telescope.nvim",
	},
	opts = {
		enable_cmp_integration = true,
	},
	config = function(_, opts)
		require("emoji").setup(opts)
		-- optional for telescope integration
		local ts = require("telescope").load_extension("emoji")
		vim.keymap.set("n", "<leader>se", ts.emoji, { desc = "[S]earch [E]moji" })
	end,
}

-- test emojie
