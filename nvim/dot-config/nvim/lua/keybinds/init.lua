local keymap = vim.keymap

-- ============================================================================
-- BASE LAYER - Most Frequent Operations
-- ============================================================================

-- File operations (very common)
keymap.set("n", "<leader>w", "<cmd>w<CR>", { desc = "Save file" })
keymap.set("n", "<leader>q", "<cmd>q<CR>", { desc = "Quit" })
keymap.set("n", "<leader>x", "<cmd>x<CR>", { desc = "Save and quit" })

-- File explorer (very common)
keymap.set("n", "<leader>e", "<cmd>Oil<CR>", { desc = "File explorer" })
keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

-- Buffer management (very common)
keymap.set("n", "<Tab>", "<cmd>bnext<CR>", { desc = "Next buffer" })
keymap.set("n", "<S-Tab>", "<cmd>bprevious<CR>", { desc = "Previous buffer" })
keymap.set("n", "<leader>x", "<cmd>bdelete<CR>", { desc = "Close buffer" })
keymap.set("n", "<leader>X", "<cmd>bdelete!<CR>", { desc = "Force close buffer" })

-- Window splits (common)
keymap.set("n", "<leader>v", "<C-w>v", { desc = "Split vertical" })
keymap.set("n", "<leader>h", "<C-w>s", { desc = "Split horizontal" })
keymap.set("n", "<leader>c", "<cmd>close<CR>", { desc = "Close window" })
keymap.set("n", "<leader>=", "<C-w>=", { desc = "Equal window sizes" })

-- Command palette (very common)
keymap.set("n", "<leader><leader>", "<cmd>Telescope command_center<CR>", { desc = "Command palette" })

-- Terminal (common)
keymap.set("n", "<leader>`", "<cmd>ToggleTerm<CR>", { desc = "Toggle terminal" })
keymap.set("t", "<C-q>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- Clear search
keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>", { desc = "Clear search highlights" })

-- ============================================================================
-- S - SEARCH/FIND (Very frequent)
-- ============================================================================
keymap.set("n", "<leader>sf", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
keymap.set("n", "<leader>sr", "<cmd>Telescope oldfiles<cr>", { desc = "Recent files" })
keymap.set("n", "<leader>sg", "<cmd>Telescope live_grep<cr>", { desc = "Grep text" })
keymap.set("n", "<leader>sw", "<cmd>Telescope grep_string<cr>", { desc = "Find word under cursor" })
keymap.set("n", "<leader>sb", "<cmd>Telescope buffers<cr>", { desc = "Find buffers" })
keymap.set("n", "<leader>sh", "<cmd>Telescope help_tags<cr>", { desc = "Help tags" })
keymap.set("n", "<leader>sk", "<cmd>Telescope keymaps<cr>", { desc = "Keymaps" })
keymap.set("n", "<leader>sp", "<cmd>Telescope projects<cr>", { desc = "Projects" })
keymap.set("n", "<leader>sn", "<cmd>Telescope notify<cr>", { desc = "Notifications" })
keymap.set("n", "<leader>sc", "<cmd>Telescope colorscheme<cr>", { desc = "Colorschemes" })

-- ============================================================================
-- G - GIT (Frequent for git users)
-- ============================================================================
keymap.set("n", "<leader>gg", "<cmd>LazyGit<CR>", { desc = "LazyGit" })
keymap.set("n", "<leader>gb", "<cmd>Gitsigns blame_line<CR>", { desc = "Git blame line" })
keymap.set("n", "<leader>gd", "<cmd>Gitsigns diffthis<CR>", { desc = "Git diff" })
keymap.set("n", "<leader>gp", "<cmd>Gitsigns preview_hunk<CR>", { desc = "Preview hunk" })
keymap.set("n", "<leader>gs", "<cmd>Gitsigns stage_hunk<CR>", { desc = "Stage hunk" })
keymap.set("n", "<leader>gu", "<cmd>Gitsigns undo_stage_hunk<CR>", { desc = "Undo stage hunk" })
keymap.set("n", "<leader>gr", "<cmd>Gitsigns reset_hunk<CR>", { desc = "Reset hunk" })
keymap.set("n", "]g", "<cmd>Gitsigns next_hunk<CR>", { desc = "Next git hunk" })
keymap.set("n", "[g", "<cmd>Gitsigns prev_hunk<CR>", { desc = "Previous git hunk" })

-- ============================================================================
-- L - LSP (Frequent when coding)
-- ============================================================================
keymap.set("n", "<leader>la", vim.lsp.buf.code_action, { desc = "Code action" })
keymap.set("v", "<leader>la", vim.lsp.buf.code_action, { desc = "Code action" })
keymap.set("n", "<leader>lr", vim.lsp.buf.rename, { desc = "Rename" })
keymap.set("n", "<leader>lf", "<cmd>lua vim.lsp.buf.format()<CR>", { desc = "Format" })
keymap.set("v", "<leader>lf", "<cmd>lua vim.lsp.buf.format()<CR>", { desc = "Format selection" })
keymap.set("n", "<leader>ld", "<cmd>Telescope diagnostics bufnr=0<CR>", { desc = "Document diagnostics" })
keymap.set("n", "<leader>lD", "<cmd>Telescope diagnostics<CR>", { desc = "Workspace diagnostics" })
keymap.set("n", "<leader>li", "<cmd>LspInfo<CR>", { desc = "LSP info" })
keymap.set("n", "<leader>lR", "<cmd>LspRestart<CR>", { desc = "LSP restart" })

-- Python venv management
keymap.set("n", "<leader>lv", "<cmd>VenvSelect<cr>", { desc = "Select Python venv" })
keymap.set("n", "<leader>lV", "<cmd>VenvSelectCached<cr>", { desc = "Select cached venv" })

-- LSP navigation (keep on g prefix for "go to")
keymap.set("n", "gd", "<cmd>Telescope lsp_definitions<CR>", { desc = "Go to definition" })
keymap.set("n", "gD", vim.lsp.buf.declaration, { desc = "Go to declaration" })
keymap.set("n", "gr", "<cmd>Telescope lsp_references<CR>", { desc = "Go to references" })
keymap.set("n", "gi", "<cmd>Telescope lsp_implementations<CR>", { desc = "Go to implementation" })
keymap.set("n", "gt", "<cmd>Telescope lsp_type_definitions<CR>", { desc = "Go to type definition" })
keymap.set("n", "K", vim.lsp.buf.hover, { desc = "Hover documentation" })

-- Diagnostics
keymap.set("n", "[d", vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })
keymap.set("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
keymap.set("n", "gl", vim.diagnostic.open_float, { desc = "Line diagnostics" })

-- ============================================================================
-- D - DEBUG (When debugging)
-- ============================================================================
keymap.set("n", "<leader>db", "<cmd>lua require('dap').toggle_breakpoint()<CR>", { desc = "Toggle breakpoint" })
keymap.set("n", "<leader>dc", "<cmd>lua require('dap').continue()<CR>", { desc = "Continue" })
keymap.set("n", "<leader>di", "<cmd>lua require('dap').step_into()<CR>", { desc = "Step into" })
keymap.set("n", "<leader>do", "<cmd>lua require('dap').step_over()<CR>", { desc = "Step over" })
keymap.set("n", "<leader>dO", "<cmd>lua require('dap').step_out()<CR>", { desc = "Step out" })
keymap.set("n", "<leader>dr", "<cmd>lua require('dap').repl.toggle()<CR>", { desc = "Toggle REPL" })
keymap.set("n", "<leader>dl", "<cmd>lua require('dap').run_last()<CR>", { desc = "Run last" })
keymap.set("n", "<leader>du", "<cmd>lua require('dapui').toggle()<CR>", { desc = "Toggle UI" })
keymap.set("n", "<leader>dt", "<cmd>lua require('dap').terminate()<CR>", { desc = "Terminate" })

-- ============================================================================
-- A - AI (When using AI)
-- ============================================================================
keymap.set("n", "<leader>aa", "<cmd>CodeCompanionActions<cr>", { desc = "AI actions" })
keymap.set("v", "<leader>aa", "<cmd>CodeCompanionActions<cr>", { desc = "AI actions" })
keymap.set("n", "<leader>ac", "<cmd>CodeCompanionChat Toggle<cr>", { desc = "AI chat" })
keymap.set("v", "<leader>ac", "<cmd>CodeCompanionChat Toggle<cr>", { desc = "AI chat" })
keymap.set("n", "<leader>ai", "<cmd>CodeCompanion<cr>", { desc = "AI inline" })
keymap.set("v", "<leader>ai", "<cmd>CodeCompanion<cr>", { desc = "AI inline" })

-- ============================================================================
-- N - NOTES (When taking notes)
-- ============================================================================
keymap.set("n", "<leader>nn", "<cmd>ObsidianNew<cr>", { desc = "New note" })
keymap.set("n", "<leader>no", "<cmd>ObsidianOpen<cr>", { desc = "Open in Obsidian" })
keymap.set("n", "<leader>ns", "<cmd>ObsidianSearch<cr>", { desc = "Search notes" })
keymap.set("n", "<leader>nq", "<cmd>ObsidianQuickSwitch<cr>", { desc = "Quick switch" })
keymap.set("n", "<leader>nt", "<cmd>ObsidianToday<cr>", { desc = "Today's note" })
keymap.set("n", "<leader>ny", "<cmd>ObsidianYesterday<cr>", { desc = "Yesterday's note" })

-- ============================================================================
-- U - UI/UTILITY
-- ============================================================================
keymap.set("n", "<leader>un", "<cmd>lua require('notify').dismiss({ silent = true, pending = true })<CR>", { desc = "Dismiss notifications" })
keymap.set("n", "<leader>ul", "<cmd>Lazy<CR>", { desc = "Lazy" })
keymap.set("n", "<leader>um", "<cmd>Mason<CR>", { desc = "Mason" })

-- ============================================================================
-- WINDOW NAVIGATION (Very frequent)
-- ============================================================================
keymap.set("n", "<C-h>", "<C-w>h", { desc = "Move to left window" })
keymap.set("n", "<C-j>", "<C-w>j", { desc = "Move to bottom window" })
keymap.set("n", "<C-k>", "<C-w>k", { desc = "Move to top window" })
keymap.set("n", "<C-l>", "<C-w>l", { desc = "Move to right window" })

-- Resize windows
keymap.set("n", "<C-Up>", "<cmd>resize +2<CR>", { desc = "Increase height" })
keymap.set("n", "<C-Down>", "<cmd>resize -2<CR>", { desc = "Decrease height" })
keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<CR>", { desc = "Decrease width" })
keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<CR>", { desc = "Increase width" })

-- ============================================================================
-- TEXT EDITING (Frequent)
-- ============================================================================

-- Better indenting
keymap.set("v", "<", "<gv", { desc = "Indent left" })
keymap.set("v", ">", ">gv", { desc = "Indent right" })

-- Move text up and down
keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move text down" })
keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move text up" })

-- Keep cursor centered when scrolling
keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll down centered" })
keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll up centered" })
keymap.set("n", "n", "nzzzv", { desc = "Next search centered" })
keymap.set("n", "N", "Nzzzv", { desc = "Previous search centered" })

-- Paste without yanking
keymap.set("x", "<leader>p", '"_dP', { desc = "Paste without yank" })

-- Delete without yanking
keymap.set({ "n", "v" }, "<leader>d", '"_d', { desc = "Delete without yank" })

-- Yank to system clipboard
keymap.set({ "n", "v" }, "<leader>y", '"+y', { desc = "Yank to clipboard" })
keymap.set("n", "<leader>Y", '"+Y', { desc = "Yank line to clipboard" })
