return {
  "nvim-telescope/telescope.nvim",
  branch = "0.1.x",
  cmd = "Telescope",
  keys = {
    { "<leader>sf", "<cmd>Telescope find_files<cr>", desc = "Find files" },
    { "<leader>sr", "<cmd>Telescope oldfiles<cr>", desc = "Recent files" },
    { "<leader>sg", "<cmd>Telescope live_grep<cr>", desc = "Grep text" },
    { "<leader>sw", "<cmd>Telescope grep_string<cr>", desc = "Find word under cursor" },
    { "<leader>sb", "<cmd>Telescope buffers<cr>", desc = "Find buffers" },
    { "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Help tags" },
    { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
    { "<leader>sp", "<cmd>Telescope projects<cr>", desc = "Projects" },
    { "<leader>sn", "<cmd>Telescope notify<cr>", desc = "Notifications" },
    { "<leader>sc", "<cmd>Telescope colorscheme<cr>", desc = "Colorschemes" },
    { "gd", "<cmd>Telescope lsp_definitions<CR>", desc = "Go to definition" },
    { "gr", "<cmd>Telescope lsp_references<CR>", desc = "Go to references" },
    { "gi", "<cmd>Telescope lsp_implementations<CR>", desc = "Go to implementation" },
    { "gt", "<cmd>Telescope lsp_type_definitions<CR>", desc = "Go to type definition" },
    { "<leader>ld", "<cmd>Telescope diagnostics bufnr=0<CR>", desc = "Document diagnostics" },
    { "<leader>lD", "<cmd>Telescope diagnostics<CR>", desc = "Workspace diagnostics" },
    { "<leader><leader>", "<cmd>Telescope commander<CR>", desc = "Command palette" },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    "nvim-tree/nvim-web-devicons",
    "rcarriga/nvim-notify",
    "ahmedkhalf/project.nvim",
    "FeiyouG/commander.nvim",
  },
  config = function()
    local telescope = require("telescope")
    local actions = require("telescope.actions")

    telescope.setup({
      defaults = {
        path_display = { "truncate" },
        mappings = {
          i = {
            ["<C-k>"] = actions.move_selection_previous,
            ["<C-j>"] = actions.move_selection_next,
            ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
          },
        },
      },
    })

    -- Load extensions safely
    pcall(telescope.load_extension, "fzf")
    pcall(telescope.load_extension, "notify")
    pcall(telescope.load_extension, "commander")
    pcall(telescope.load_extension, "projects")
  end,
}
