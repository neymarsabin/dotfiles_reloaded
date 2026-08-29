-- All truecolor-correct dark themes live here. The active one is persisted to
-- stdpath("data")/theme.txt by the <leader>u switcher (see ui.lua) so your
-- choice survives restarts. Default = tokyonight-storm.

local PERSIST = vim.fn.stdpath("data") .. "/theme.txt"
local DEFAULT = "tokyonight-storm"

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

-- lazy.nvim does not load a `lazy = true` theme just because :colorscheme names
-- it, so at startup the saved scheme resolves to nothing and neovim silently
-- keeps its default. Find whichever plugin ships colors/<name> and load it
-- first. Only tokyonight worked before this, being the one lazy = false spec.
local function ensure_loaded(name)
	if vim.fn.getcompletion(name, "color")[1] then
		return
	end

	for plugin, spec in pairs(require("lazy.core.config").plugins) do
		local dir = spec.dir or ""
		if
			vim.fn.filereadable(dir .. "/colors/" .. name .. ".lua") == 1
			or vim.fn.filereadable(dir .. "/colors/" .. name .. ".vim") == 1
		then
			pcall(require("lazy").load, { plugins = { plugin } })
			return
		end
	end
end

return {
	-- ── tokyonight (default) ────────────────────────────────────────────────
	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 1000,
		opts = {
			style = "storm", -- storm = lighter bg than night, less glare
			transparent = false,
			styles = {
				comments = { italic = true },
				keywords = { italic = true },
			},
			on_highlights = function(hl, c)
				-- brighter window separators so splits read clearly
				hl.WinSeparator = { fg = c.blue0, bold = true }
				-- storm's default comment (#565f89) is 2.35:1 against its
				-- lighter bg, under the 3:1 floor; lift it to ~4.8:1
				hl.Comment = { fg = "#8b93b8", italic = true }
			end,
		},
		config = function(_, opts)
			require("tokyonight").setup(opts)
			-- apply the persisted theme once everything is loaded
			vim.schedule(function()
				local theme = saved_theme()
				ensure_loaded(theme)
				local ok, err = pcall(vim.cmd.colorscheme, theme)
				if not ok then
					vim.notify("Theme " .. theme .. " failed: " .. tostring(err), vim.log.levels.WARN)
				end
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

	-- ── extra theme packs ───────────────────────────────────────────────────
	-- installed purely to give <leader>ut something to browse; each is lazy and
	-- costs nothing until the picker or a saved theme pulls it in
	{ "EdenEast/nightfox.nvim", lazy = true }, -- nightfox dayfox dawnfox duskfox nordfox terafox carbonfox
	{ "projekt0n/github-nvim-theme", lazy = true }, -- github_dark, _dimmed, _default, _high_contrast, light variants
	{ "olimorris/onedarkpro.nvim", lazy = true }, -- onedark onelight onedark_vivid onedark_dark
	{ "Mofiqul/dracula.nvim", lazy = true }, -- dracula dracula-soft
	{ "Mofiqul/vscode.nvim", lazy = true }, -- vscode, for when the old editor is missed
	{ "navarasu/onedark.nvim", lazy = true },
	{ "rmehri01/onenord.nvim", lazy = true },
	{ "AlexvZyl/nordic.nvim", lazy = true }, -- maintained nord, replaces gbprod/nord.nvim
	{ "nyoom-engineering/oxocarbon.nvim", lazy = true },
	{ "sainnhe/everforest", lazy = true },
	{ "sainnhe/sonokai", lazy = true },
	{ "sainnhe/edge", lazy = true },
	{ "savq/melange-nvim", lazy = true },
	{ "vague2k/vague.nvim", lazy = true },
	{ "scottmckendry/cyberdream.nvim", lazy = true },
	{ "marko-cerovac/material.nvim", lazy = true },
	{ "bluz71/vim-nightfly-colors", name = "nightfly", lazy = true },
	{ "bluz71/vim-moonfly-colors", name = "moonfly", lazy = true },
	{ "ribru17/bamboo.nvim", lazy = true },
	{ "0xstepit/flow.nvim", lazy = true },
}
