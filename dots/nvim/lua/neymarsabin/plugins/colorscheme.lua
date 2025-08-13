--
-- Using Lazy
-- return {
-- 	"navarasu/onedark.nvim",
-- 	priority = 1000, -- make sure to load this before all the other start plugins
-- 	config = function()
-- 		require("onedark").setup({
-- 			style = "deep",
-- 		})
-- 		-- Enable theme
-- 		require("onedark").load()
-- 	end,
-- }
-- return {
-- 	{
-- 		"ellisonleao/gruvbox.nvim",
-- 		config = function()
-- 			require("gruvbox").setup({
-- 				undercurl = true,
-- 				underline = true,
-- 				bold = true,
-- 				italic = {
-- 					strings = true,
-- 					comments = true,
-- 					operators = false,
-- 					folds = false,
-- 				},
-- 				strikethrough = true,
-- 				invert_selection = false,
-- 				invert_signs = false,
-- 				invert_tabline = false,
-- 				invert_intend_guides = false,
-- 				contrast = "hard", -- can be "hard", "soft" or "medium"
-- 				overrides = {},
-- 			})
-- 		end,
-- 	},
-- }
--
return {
	{
		"rose-pine/neovim",
		name = "rose-pine",
		lazy = false,
		config = function()
			require("rose-pine").setup({
				variant = "auto", -- auto, main, moon, or dawn
				dark_variant = "main", -- main, moon, or dawn
				dim_inactive_windows = false,
				extend_background_behind_borders = true,

				enable = {
					terminal = true,
					legacy_highlights = true, -- Improve compatibility for previous versions of Neovim
					migrations = true, -- Handle deprecated options automatically
				},

				styles = {
					bold = true,
					italic = false,
					transparency = false,
				},

				groups = {
					border = "muted",
					link = "iris",
					panel = "surface",

					error = "love",
					hint = "iris",
					info = "foam",
					note = "pine",
					todo = "rose",
					warn = "gold",

					git_add = "foam",
					git_change = "rose",
					git_delete = "love",
					git_dirty = "rose",
					git_ignore = "muted",
					git_merge = "iris",
					git_rename = "pine",
					git_stage = "iris",
					git_text = "rose",
					git_untracked = "subtle",

					h1 = "iris",
					h2 = "foam",
					h3 = "rose",
					h4 = "gold",
					h5 = "pine",
					h6 = "foam",
				},

				highlight_groups = {},

				before_highlight = function(group, highlight, palette) end,
			})
			vim.cmd.colorscheme("rose-pine-main")
		end,
	},
}
