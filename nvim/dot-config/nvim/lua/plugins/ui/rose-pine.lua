return {
  "rose-pine/neovim",
  name = "rose-pine",
  priority = 1000,
  config = function()
    require("rose-pine").setup({
      variant = "main",
      dark_variant = "main",
      dim_inactive_windows = false,
      extend_background_behind_borders = true,

      styles = {
        bold = true,
        italic = true,
        transparency = false,
      },

      highlight_groups = {
        Comment = { italic = true },
        Keyword = { italic = true },
        Type = { italic = true },
        StorageClass = { italic = true },
        Structure = { italic = true },
        Parameter = { italic = true },
        Function = { italic = true },
        ["@parameter"] = { italic = true },
        ["@variable"] = { italic = true },
        ["@type.builtin"] = { italic = true },
      },
    })

    vim.cmd("colorscheme rose-pine")
  end,
}
