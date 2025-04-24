-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()
config.enable_tab_bar = false
config.audible_bell = "Disabled"
config.warn_about_missing_glyphs = false
config.font = wezterm.font("JetBrains Mono")
config.cursor_blink_rate = 800
config.force_reverse_video_cursor = true
config.color_scheme = "Tokyo Night"

-- and finally, return the configuration to wezterm
return config
