return {
	"folke/flash.nvim",
	event = "VeryLazy",
	opts = {
		modes = {
			search = { enabled = false },
			char = { enabled = false },
		},
	},
	keys = {
		{ "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash jump" },
		{ "S", mode = { "n", "x", "o" }, function() require("flash").treesitter() end, desc = "Flash treesitter select" },
		{ "r", mode = "o", function() require("flash").remote() end, desc = "Remote flash" },
		{ "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter search" },
	},
}
