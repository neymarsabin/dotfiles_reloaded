-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()
config.enable_tab_bar = false
config.audible_bell = "Disabled"
config.color_scheme = "Tokyo Night"
config.warn_about_missing_glyphs = false

-- and finally, return the configuration to wezterm
return config
