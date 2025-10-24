local telescope = require('telescope.builtin')
vim.g.leader = " "
vim.g.localleader = " "
vim.keymap.set("n", "<leader>ff", telescope.find_files, { desc = "Find Files" })
