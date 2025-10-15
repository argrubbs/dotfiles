return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
  end,
  config = function()
    local wk = require("which-key")
    wk.setup({})

    wk.add({
      { "<leader>s", group = "search" },
      { "<leader>g", group = "git" },
      { "<leader>l", group = "lsp" },
      { "<leader>d", group = "debug" },
      { "<leader>a", group = "ai" },
      { "<leader>n", group = "notes" },
      { "<leader>u", group = "ui" },
    })
  end,
}
