return {
  "stevearc/dressing.nvim",
  event = "VeryLazy",
  config = function()
    require("dressing").setup({
      input = {
        enabled = true,
        default_prompt = "Input:",
        prompt_align = "left",
        insert_only = true,
        start_in_insert = true,
        border = "rounded",
        relative = "cursor",
        prefer_width = 40,
        width = nil,
        max_width = { 140, 0.9 },
        min_width = { 20, 0.2 },
        win_options = {
          winblend = 10,
          wrap = false,
        },
        mappings = {
          n = {
            ["<Esc>"] = "Close",
            ["<CR>"] = "Confirm",
          },
          i = {
            ["<C-c>"] = "Close",
            ["<CR>"] = "Confirm",
            ["<Up>"] = "HistoryPrev",
            ["<Down>"] = "HistoryNext",
          },
        },
        override = function(conf)
          return conf
        end,
        get_config = nil,
      },
      select = {
        enabled = true,
        backend = { "telescope", "fzf_lua", "fzf", "builtin", "nui" },
        trim_prompt = true,
        telescope = require("telescope.themes").get_cursor(),
        fzf = {
          window = {
            width = 0.5,
            height = 0.4,
          },
        },
        builtin = {
          border = "rounded",
          relative = "editor",
          win_options = {
            winblend = 10,
          },
          width = nil,
          max_width = { 140, 0.8 },
          min_width = { 40, 0.2 },
          height = nil,
          max_height = 0.9,
          min_height = { 10, 0.2 },
        },
        format_item_override = {},
        get_config = nil,
      },
    })
  end,
}
