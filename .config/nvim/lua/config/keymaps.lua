-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local map = vim.keymap
-- Config reload
map.set("n", "<leader>r", ":source $MYVIMRC<CR>", { desc = "Reload config" })

-- Oil Nvim
map.set("n", "<leader>fd", ":Oil<CR>", { desc = "File Manager (Oil)" })
