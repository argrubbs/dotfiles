return {
  "goolord/alpha-nvim",
  event = "VimEnter",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local alpha = require("alpha")
    local dashboard = require("alpha.themes.dashboard")

    -- Set header with custom text: "Why Don't You Zamboni"
    dashboard.section.header.val = {
      [[                                                                                   ]],
      [[  ██╗    ██╗██╗  ██╗██╗   ██╗    ██████╗  ██████╗ ███╗   ██╗    ████████╗       ]],
      [[  ██║    ██║██║  ██║╚██╗ ██╔╝    ██╔══██╗██╔═══██╗████╗  ██║    ╚══██╔══╝       ]],
      [[  ██║ █╗ ██║███████║ ╚████╔╝     ██║  ██║██║   ██║██╔██╗ ██║       ██║          ]],
      [[  ██║███╗██║██╔══██║  ╚██╔╝      ██║  ██║██║   ██║██║╚██╗██║       ██║          ]],
      [[  ╚███╔███╔╝██║  ██║   ██║       ██████╔╝╚██████╔╝██║ ╚████║       ██║          ]],
      [[   ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝       ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝       ╚═╝          ]],
      [[                                                                                   ]],
      [[  ██╗   ██╗ ██████╗ ██╗   ██╗    ███████╗ █████╗ ███╗   ███╗██████╗  ██████╗ ███╗   ██╗██╗]],
      [[  ╚██╗ ██╔╝██╔═══██╗██║   ██║    ╚══███╔╝██╔══██╗████╗ ████║██╔══██╗██╔═══██╗████╗  ██║██║]],
      [[   ╚████╔╝ ██║   ██║██║   ██║      ███╔╝ ███████║██╔████╔██║██████╔╝██║   ██║██╔██╗ ██║██║]],
      [[    ╚██╔╝  ██║   ██║██║   ██║     ███╔╝  ██╔══██║██║╚██╔╝██║██╔══██╗██║   ██║██║╚██╗██║██║]],
      [[     ██║   ╚██████╔╝╚██████╔╝    ███████╗██║  ██║██║ ╚═╝ ██║██████╔╝╚██████╔╝██║ ╚████║██║]],
      [[     ╚═╝    ╚═════╝  ╚═════╝     ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝╚═════╝  ╚═════╝ ╚═╝  ╚═══╝╚═╝]],
      [[                                                                                   ]],
    }

    -- Set menu
    dashboard.section.buttons.val = {
      dashboard.button("f", "  Find file", ":Telescope find_files<CR>"),
      dashboard.button("n", "  New file", ":ene <BAR> startinsert<CR>"),
      dashboard.button("r", "  Recent files", ":Telescope oldfiles<CR>"),
      dashboard.button("p", "  Recent projects", ":Telescope projects<CR>"),
      dashboard.button("o", "  Open folder", ":Oil<CR>"),
      dashboard.button("g", "  Find text", ":Telescope live_grep<CR>"),
      dashboard.button("c", "  Edit config", ":lua require('telescope.builtin').find_files({ cwd = vim.fn.stdpath('config') })<CR>"),
      dashboard.button("l", "  Lazy", ":Lazy<CR>"),
      dashboard.button("m", "  Mason", ":Mason<CR>"),
      dashboard.button("q", "  Quit", ":qa<CR>"),
    }

    -- Set footer
    local function footer()
      local datetime = os.date(" %Y-%m-%d   %H:%M:%S")
      local version = vim.version()
      local nvim_version_info = "   v" .. version.major .. "." .. version.minor .. "." .. version.patch
      return datetime .. "   " .. nvim_version_info
    end

    dashboard.section.footer.val = footer()

    -- Layout
    dashboard.config.layout = {
      { type = "padding", val = 2 },
      dashboard.section.header,
      { type = "padding", val = 2 },
      dashboard.section.buttons,
      { type = "padding", val = 1 },
      dashboard.section.footer,
    }

    dashboard.config.opts.noautocmd = true

    -- Disable folding on alpha buffer
    vim.cmd([[
      autocmd FileType alpha setlocal nofoldenable
      autocmd FileType alpha setlocal nocursorline
      autocmd FileType alpha setlocal nolist
      autocmd FileType alpha setlocal listchars=
    ]])

    -- Disable statusline in alpha
    vim.cmd([[
      autocmd FileType alpha setlocal laststatus=0
      autocmd BufUnload <buffer> setlocal laststatus=3
    ]])

    -- Send config to alpha
    alpha.setup(dashboard.config)
  end,
}
