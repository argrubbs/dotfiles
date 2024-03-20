return {
	{
		"catppuccin/nvim",
		name = "catppuccin",
		priority = 1000,
		config = function()
			local catppuccin = require("catppuccin")
			catppuccin.setup({
				flavour = "frappe",
				background = {
					light = "latte",
					dark = "frappe",
				},
				term_colors = true,
			})
		end,
	},
}
