-- Wezterm API
local wezterm = require("wezterm")

-- Wezterm Config object
local wezconf = wezterm.config_builder()

wezconf.color_scheme = 'Kanagawa',

return wezconf
