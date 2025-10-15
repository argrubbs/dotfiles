-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Set leader keys before loading plugins
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Load core configuration
require("options")
require("keybinds")
require("colorscheme")

-- Load plugins
require("lazy").setup({
  { import = "plugins.ui" },
  { import = "plugins.lsp" },
  { import = "plugins.editor" },
  { import = "plugins.coding" },
  { import = "plugins.search" },
  { import = "plugins.git" },
  { import = "plugins.ai" },
  { import = "plugins.notes" },
  { import = "plugins.debug" },
}, {
  change_detection = {
    notify = false,
  },
})
