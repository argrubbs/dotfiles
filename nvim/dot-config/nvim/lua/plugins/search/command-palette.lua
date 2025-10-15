return {
  "FeiyouG/commander.nvim",
  dependencies = { "nvim-telescope/telescope.nvim" },
  keys = {
    { "<leader><leader>", "<cmd>Telescope commander<cr>", desc = "Command palette" },
  },
  config = function()
    local commander = require("commander")

    commander.setup({
      components = {
        "DESC",
        "KEYS",
        "CAT",
      },
      sort_by = {
        "DESC",
        "KEYS",
        "CAT",
        "CMD",
      },
      integration = {
        telescope = {
          enable = true,
        },
        lazy = {
          enable = true,
        },
      },
    })

    commander.add({
      {
        desc = "Find files",
        cmd = "<CMD>Telescope find_files<CR>",
        keys = { "n", "<leader>sf" },
      },
      {
        desc = "Live grep",
        cmd = "<CMD>Telescope live_grep<CR>",
        keys = { "n", "<leader>sg" },
      },
      {
        desc = "Recent files",
        cmd = "<CMD>Telescope oldfiles<CR>",
        keys = { "n", "<leader>sr" },
      },
      {
        desc = "Find buffers",
        cmd = "<CMD>Telescope buffers<CR>",
        keys = { "n", "<leader>sb" },
      },
      {
        desc = "Find projects",
        cmd = "<CMD>Telescope projects<CR>",
        keys = { "n", "<leader>sp" },
      },
      {
        desc = "LSP: Go to definition",
        cmd = "<CMD>Telescope lsp_definitions<CR>",
        keys = { "n", "gd" },
      },
      {
        desc = "LSP: References",
        cmd = "<CMD>Telescope lsp_references<CR>",
        keys = { "n", "gr" },
      },
      {
        desc = "LSP: Code actions",
        cmd = "<CMD>lua vim.lsp.buf.code_action()<CR>",
        keys = { "n", "<leader>la" },
      },
      {
        desc = "LSP: Rename",
        cmd = "<CMD>lua vim.lsp.buf.rename()<CR>",
        keys = { "n", "<leader>lr" },
      },
      {
        desc = "Format buffer",
        cmd = "<CMD>lua vim.lsp.buf.format()<CR>",
        keys = { "n", "<leader>lf" },
      },
      {
        desc = "Python: Select venv",
        cmd = "<CMD>VenvSelect<CR>",
        keys = { "n", "<leader>lv" },
      },
      {
        desc = "Python: Select cached venv",
        cmd = "<CMD>VenvSelectCached<CR>",
        keys = { "n", "<leader>lV" },
      },
      {
        desc = "Toggle terminal",
        cmd = "<CMD>ToggleTerm<CR>",
        keys = { "n", "<leader>`" },
      },
      {
        desc = "Open LazyGit",
        cmd = "<CMD>LazyGit<CR>",
        keys = { "n", "<leader>gg" },
      },
      {
        desc = "File explorer",
        cmd = "<CMD>Oil<CR>",
        keys = { "n", "<leader>e" },
      },
      {
        desc = "Notifications",
        cmd = "<CMD>Telescope notify<CR>",
        keys = { "n", "<leader>sn" },
      },
      {
        desc = "Dismiss notifications",
        cmd = "<CMD>lua require('notify').dismiss({ silent = true, pending = true })<CR>",
        keys = { "n", "<leader>un" },
      },
      {
        desc = "AI: Chat",
        cmd = "<CMD>CodeCompanionChat Toggle<CR>",
        keys = { "n", "<leader>ac" },
      },
      {
        desc = "AI: Actions",
        cmd = "<CMD>CodeCompanionActions<CR>",
        keys = { "n", "<leader>aa" },
      },
      {
        desc = "Mason: LSP servers",
        cmd = "<CMD>Mason<CR>",
        keys = { "n", "<leader>um" },
      },
      {
        desc = "Lazy: Plugin manager",
        cmd = "<CMD>Lazy<CR>",
        keys = { "n", "<leader>ul" },
      },
    })
  end,
}
