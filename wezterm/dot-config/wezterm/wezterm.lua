-- Wezterm API
local wezterm = require("wezterm")
local mux = wezterm.mux

-- Wezterm Config object
local wezconf = wezterm.config_builder()

wezterm.on("gui-startup", function(cmd)
	local tab, pane, window = mux.spawn_window(cmd or {})
	window:gui_window():maximize()
end)

wezconf.color_scheme = "Gruvbox dark, medium (base16)"

wezconf.window_decorations = "RESIZE"
wezconf.enable_tab_bar = false
wezconf.font_size = 19.0
wezconf.font = wezterm.font("MesloLGS Nerd Font Mono")

return wezconf
