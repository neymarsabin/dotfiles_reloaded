-- All truecolor-correct dark themes live here. The active one is persisted to
-- stdpath("data")/theme.txt by the <leader>u switcher (see ui.lua) so your
-- choice survives restarts. Default = kanagawa-wave.

local PERSIST = vim.fn.stdpath("data") .. "/theme.txt"
local DEFAULT = "kanagawa-wave"

-- read the saved colorscheme name, falling back to the default
local function saved_theme()
	local f = io.open(PERSIST, "r")
	if not f then
		return DEFAULT
	end
	local name = vim.trim(f:read("*a") or "")
	f:close()
	return name ~= "" and name or DEFAULT
end

-- expose for the switcher
_G.ThemePersistPath = PERSIST

return {
	-- ── tokyonight (default) ────────────────────────────────────────────────
	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 1000,
		opts = {
			style = "night", -- night = darkest variant
			transparent = false,
			styles = {
				comments = { italic = true },
				keywords = { italic = true },
			},
			on_highlights = function(hl, c)
				-- brighter window separators so splits read clearly
				hl.WinSeparator = { fg = c.blue0, bold = true }
			end,
		},
		config = function(_, opts)
			require("tokyonight").setup(opts)
			-- apply the persisted theme once everything is loaded
			vim.schedule(function()
				pcall(vim.cmd.colorscheme, saved_theme())
			end)
		end,
	},

	-- ── catppuccin ──────────────────────────────────────────────────────────
	{
		"catppuccin/nvim",
		name = "catppuccin",
		lazy = true,
		opts = {
			flavour = "mocha",
			term_colors = true,
			integrations = {
				cmp = true,
				gitsigns = true,
				treesitter = true,
				telescope = { enabled = true },
				which_key = true,
				indent_blankline = { enabled = true },
				mason = true,
				neogit = true,
				harpoon = true,
				native_lsp = { enabled = true, underlines = {
					errors = { "undercurl" },
					hints = { "undercurl" },
					warnings = { "undercurl" },
					information = { "undercurl" },
				} },
				mini = { enabled = true },
			},
		},
	},

	-- ── kanagawa ──────────────────────────────────────────────────────────────
	{
		"rebelot/kanagawa.nvim",
		lazy = true,
		opts = {
			theme = "wave", -- wave (default dark), dragon (darker), lotus (light)
			transparent = false,
			dimInactive = true,
		},
	},

	-- ── rose-pine ─────────────────────────────────────────────────────────────
	{
		"rose-pine/neovim",
		name = "rose-pine",
		lazy = true,
		opts = {
			variant = "moon", -- main | moon (darker) | dawn (light)
			dim_inactive_windows = true,
			styles = { italic = true, transparency = false },
		},
	},

	-- ── gruvbox-material ────────────────────────────────────────────────────
	{
		"sainnhe/gruvbox-material",
		lazy = true,
		config = function()
			vim.g.gruvbox_material_background = "hard"
			vim.g.gruvbox_material_foreground = "material"
			vim.g.gruvbox_material_enable_italic = 1
			vim.g.gruvbox_material_better_performance = 1
		end,
	},
}
