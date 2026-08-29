-- single letters instead of NORMAL/VISUAL/INSERT, so the mode fits in one cell
local mode_map = {
	NORMAL = "N",
	INSERT = "I",
	VISUAL = "V",
	["V-LINE"] = "VL",
	["V-BLOCK"] = "VB",
	SELECT = "S",
	REPLACE = "R",
	COMMAND = "C",
	TERMINAL = "T",
	["O-PENDING"] = "O",
}

return {
	"nvim-lualine/lualine.nvim",
	config = function()
		require("lualine").setup({
			options = {
				icons_enabled = false,
				theme = "auto", -- follows whatever colorscheme is active
				component_separators = "",
				section_separators = "",
				disabled_filetypes = {
					statusline = { "neo-tree", "neogit", "Outline" },
				},
				globalstatus = true, -- one statusline for all splits
				refresh = { statusline = 300 },
			},
			sections = {
				-- a and z are the sections lualine paints with an accent colour;
				-- pinning them to the StatusLine group keeps the mode letter and
				-- the position readable without the bold slab, and follows every
				-- colorscheme rather than hardcoding a hex
				lualine_a = {
					{
						"mode",
						fmt = function(s)
							return mode_map[s] or s:sub(1, 1)
						end,
						color = "StatusLine",
					},
				},
				lualine_b = { "branch", "diff" },
				lualine_c = { { "filename", path = 1 } },
				lualine_x = { { "diagnostics", sources = { "nvim_lsp" } }, "filetype" },
				lualine_y = { "progress" },
				lualine_z = { { "location", color = "StatusLine" } },
			},
			inactive_sections = {
				lualine_c = { { "filename", path = 1 } },
				lualine_x = { "location" },
			},
		})
	end,
}
