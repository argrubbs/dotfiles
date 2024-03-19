return {
  --  {
  --    'echasnovski/mini.nvim',
  --  version = '*'
  --  },
  --  {
  --    'echasnovski/mini.surround',
  --    version = '*',
  --    config = function()
  --      require('mini.surround').setup()
  --    end,
  --  },
  {
    "kylechui/nvim-surround",
    version = '*',
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
        -- 1config1 here or leave blank
      })
    end,
  },
}
