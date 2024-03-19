return {
	"nvim-telescope/telescope.nvim",
	branch = "0.1.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		"nvim-tree/nvim-web-devicons",
	},
	config = function()
		local telescope = require("telescope")
		local actions = require("telescope.actions")

		telescope.setup({
			defaults = {
				mappings = {
					i = {
						["<C-k>"] = actions.move_selection_previous,
						["<C-j>"] = actions.move_selection_next,
						["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
					},
				},
			},
		})

		telescope.load_extension("fzf")

		local keymap = vim.keymap

		-- Find Files Keymaps
		keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<CR>", { desc = "Find files (cwd)" })
		keymap.set("n", "<leader>fr", "<cmd>Telescope oldfiles<CR>", { desc = "Find recent files" })
		keymap.set("n", "<leader>fG", "<cmd>Telescope git_files<cr>", { desc = "Find Files in Git repo" })
		keymap.set("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", { desc = "Find recent files" })

		-- Search files keymaps
		keymap.set("n", "<leader>sg", "<cmd>Telescope live_grep<CR>", { desc = "Live Grep (cwd)" })
		keymap.set("n", "<leader>sc", "<cmd>Telescope grep_string<CR>", { desc = "Find string under cursor (cwd)" })
		keymap.set("n", "<leader>sk", "<cmd>Telescope keymaps<CR>", { desc = "Find keymaps" })
		keymap.set("n", "<leader>sG", "<cmd>Telescope git_status<cr>", { desc = "Search Git Status" })
	end,
}
