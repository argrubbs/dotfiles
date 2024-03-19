vim.g.mapleader = " "

local keymap = vim.keymap -- for ease of use

-- general keymaps

keymap.set("i", "jk", "<ESC>", { desc = "Easy home row ESC key" }) -- easy home row ESC key
keymap.set("n", "<leader>nh", ":nohl<CR>", { desc = "Remove search highlight" }) -- remove search highlight
keymap.set("n", "x", '"_x', { desc = 'Map "x" to erase without register' }) -- Maps 'x' key to erase but doesn't copy to register

-- Use leader and + or - to incrememt and decremeent numbers
keymap.set("n", "<leader>+", "<C-a>", { desc = "Increment number under cursor" })
keymap.set("n", "<leader>-", "<C-x>", { desc = "Decrement number under cursor" })

-- Window Splits
keymap.set("n", "<leader>wv", "<C-w>v", { desc = "Split window vertically" }) -- vertical split
keymap.set("n", "<leader>wh", "<C-w>s", { desc = "Split window horizontally" }) -- horizontal split
keymap.set("n", "<leader>we", "<C-w>=", { desc = "Make splits equal width" }) -- make splits equal width
keymap.set("n", "<leader>wx", ":close<CR>", { desc = "Close current split window" }) -- close current split window

keymap.set("n", "<leader><TAB><TAB>", ":tabnew<CR>", { desc = "New Tab" }) -- New Tab
keymap.set("n", "<leader><TAB>x", ":tabclose<CR>", { desc = "Close Tab" }) -- Close Tab
keymap.set("n", "<leader><TAB>n", ":tabn<CR>", { desc = "Next Tab" }) -- Next Tab
keymap.set("n", "<leader><TAB>b", ":tabp<CR>", { desc = "Previous Tab" }) -- Previous Tab

-- plugin keymaps

keymap.set("n", "<leader>wm", ":MaximizerToggle<CR>", { desc = "Toggle Maximize Split" })
