-- Pull in the wezterm API
local wezterm = require("wezterm")
local act = wezterm.action

-- This will hold the configuration.
--
local config = wezterm.config_builder()

-- ─── Theme ───────────────────────────────────────────────────────────────────
-- WezTerm passes truecolor by default, so neovim owns its own colors. This
-- scheme only styles the *terminal* (ANSI palette + the padding background) so
-- there is no visible seam around the nvim window. Matches tokyonight-night.
config.color_scheme = "Tokyo Night"

-- ─── Font ─────────────────────────────────────────────────────────────────────
config.font = wezterm.font("JetBrains Mono", { weight = "Regular" })
config.font_size = 14.0
config.line_height = 1.2
config.cell_width = 1.0

-- ─── Cursor ───────────────────────────────────────────────────────────────────
config.cursor_blink_rate = 800
config.default_cursor_style = "BlinkingBlock"
-- Let catppuccin control cursor color instead of reverse video
config.force_reverse_video_cursor = false

-- ─── Window ───────────────────────────────────────────────────────────────────
config.window_decorations = "RESIZE"
config.window_padding = {
	left = 12,
	right = 12,
	top = 10,
	bottom = 10,
}
config.window_background_opacity = 1.0
config.macos_window_background_blur = 0

-- ─── Tab bar ──────────────────────────────────────────────────────────────────
config.enable_tab_bar = false

-- ─── Misc ─────────────────────────────────────────────────────────────────────
config.audible_bell = "Disabled"
config.warn_about_missing_glyphs = false
config.scrollback_lines = 10000

-- ─── macOS Option/Alt handling ────────────────────────────────────────────────
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = false

-- Force Option+v / Option+d to send Alt-style sequences
config.keys = {
	{ key = "v", mods = "OPT", action = act.SendString("\x1bv") },
	{ key = "d", mods = "OPT", action = act.SendString("\x1bd") },
}

-- and finally, return the configuration to wezterm
return config
