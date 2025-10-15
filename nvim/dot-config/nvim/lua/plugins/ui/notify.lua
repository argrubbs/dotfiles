return {
  "rcarriga/nvim-notify",
  lazy = false,
  priority = 900,
  config = function()
    local notify = require("notify")
    notify.setup({
      stages = "fade_in_slide_out",
      timeout = 3000,
      background_colour = "#000000",
      icons = {
        ERROR = "",
        WARN = "",
        INFO = "",
        DEBUG = "",
        TRACE = "✎",
      },
      top_down = true,
      max_width = 50,
      max_height = 10,
      render = "compact",
    })

    vim.notify = notify

    -- Keymaps are defined in lua/keybinds/init.lua
  end,
}
