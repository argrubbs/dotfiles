local opt = vim.opt

-- Line numbers
opt.number = true
opt.relativenumber = true

-- Tabs and indentation
opt.tabstop = 2
opt.shiftwidth = 2
opt.expandtab = true
opt.autoindent = true
opt.smartindent = true

-- Line wrapping
opt.wrap = false

-- Search settings
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

-- Appearance
opt.termguicolors = true
opt.background = "dark"
opt.signcolumn = "yes"
opt.cursorline = true
opt.colorcolumn = "120"

-- Backspace
opt.backspace = "indent,eol,start"

-- Clipboard
opt.clipboard:append("unnamedplus")

-- Split windows
opt.splitright = true
opt.splitbelow = true

-- Swap and backup
opt.swapfile = false
opt.backup = false
opt.undofile = true
opt.undodir = vim.fn.expand("~/.vim/undodir")

-- Scrolling
opt.scrolloff = 8
opt.sidescrolloff = 8

-- Update time
opt.updatetime = 250
opt.timeoutlen = 300

-- Completion
opt.completeopt = "menuone,noselect"

-- File encoding
opt.fileencoding = "utf-8"

-- Mouse
opt.mouse = "a"

-- Performance
opt.lazyredraw = false

-- Folds
opt.foldmethod = "expr"
opt.foldexpr = "nvim_treesitter#foldexpr()"
opt.foldlevel = 99
opt.foldenable = false

-- Concealment for prettier markdown/latex
opt.conceallevel = 2
