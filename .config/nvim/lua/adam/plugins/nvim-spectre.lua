return {
	{
		"nvim-pack/nvim-spectre",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		config = function()
			local keymap = vim.keymap

			keymap.set("n", "<leader>sr", '<cmd>lua require("spectre").toggle()<CR>', { desc = "Toggle Spectre" })
			keymap.set(
				"n",
				"<leader>sw",
				'<cmd>lua require("spectre").open_visual({select_word=treuw})<CR>',
				{ desc = "Search current word" }
			)
			keymap.set(
				"v",
				"<leader>sw",
				'<esc><cmd>lua require("spectre").open_visual()<CR>',
				{ desc = "Search Current Word" }
			)
			keymap.set(
				"n",
				"<leader>sp",
				'<cmd>lua require("spectre").open_file_search({select_word=true})<CR>',
				{ desc = "Search on Current File" }
			)
		end,
	},
}
