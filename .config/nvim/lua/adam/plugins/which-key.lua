return {
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
		end,
		opts = {},
		config = function()
			local wk = require("which-key")
			wk.register({
				["<leader>"] = {
					f = {
						name = "+File",
					},
					w = {
						name = "+Window",
					},
					s = {
						name = "+Search",
					},
					["<TAB>"] = {
						name = "+Tabs",
					},
					n = {
						name = "+Remove",
					},
					e = {
						name = "+Explorer",
					},
				},
			})
		end,
	},
}
