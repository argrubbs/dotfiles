return {
	{
		"nvim-tree/nvim-tree.lua",
		version = "*",
		lazy = false,
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			-- recommended settings from nvim-tree docs
			vim.g.loaded_netrw = 1
			vim.g.loaded_netrwPlugin = 1

			-- change color for arrows in tree to light blue
			vim.cmd([[ highlight NvimTreeIndentMarker guifg=#3FC5FF ]])

			-- configure nvim-tree
			require("nvim-tree").setup({
				sort = {
					sorter = "case_sensitive",
				},
				view = {
					width = 30,
				},
				renderer = {
					group_empty = true,
				},
				filters = {
					dotfiles = true,
				},
				git = {
					ignore = false,
				},
			})
			-- set keymaps
			local keymap = vim.keymap

			keymap.set("n", "<leader>ee", "<cmd>NvimTreeToggle<CR>", { desc = "Toggle File Explorer" })
			keymap.set("n", "<leader>ef", "<cmd>NvimTreeFindFileToggle<CR>", { desc = "Toggle NvimTree Find File" })
			keymap.set("n", "<leader>ec", "<cmd>NvimTreeCollapse<CR>", { desc = "Collapse Folders in File Explorer" })
			keymap.set("n", "<leader>er", "<cmd>NvimTreeRefresh<CR>", { desc = "Refresh File Explorer" })
		end,
	},
}
