-- Wezterm API
local wezterm = require("wezterm")

-- Wezterm Config object
local wezconf = wezterm.config_builder()

wezconf.color_scheme = "rose-pine"

wezconf.window_decorations = "RESIZE"
wezconf.enable_tab_bar = false
wezconf.font_size = 19.0
wezconf.font = wezterm.font("MesloLGS Nerd Font Mono")

return wezconf
