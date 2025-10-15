# Overview
This project is for a Neovim configuration. Unless otherwise specified, the language to use is Lua.

# Components
- Lazy.nvim for package management
- Essential Neovim configuration settings
- LSP functionality with Mason integration
- Debugging functionality
- Treesitter and text object support
- Note taking and documentation functions
- AI code integration using ollama and claude
- Support for IDE functions for Python, Go, Ansible, Terraform
- Git integration using lazygit
- Telescope fuzzy finders for as many things as possible

# UI Details
- Rose Pine colorscheme
- Hack Nerd Font with font size 17
- Use of cursive stylized font for comments, variables, function signature components (where appropriate)
- Functional and good-looking statusline
- Gitsigns integration
- Colored CSV and JSON text highlighting

# Editor Details
- Flash.nvim for movement
- Surround plugin for quotes, parens, etc.
- Matching autopairs
- Indentation guides
- File management using oil.nvim
- No filetree, use Telescope/fuzzy finders instead

# File structure
- root
    - lua
        - plugins
        - options
        - keybinds
        - colorscheme

Plugins should be organized in folders based on a category type of the plugin. Example: plugins/search/telescope.lua or plugins/lsp/mason.lua
- I would like a popup for messages/notifications. Something that appears in the top-right corner to inform me of things. Also, make sure there's a telescope option for viewing notifications and messages
- Add functionality for the command input to be a floating box with autocomplete suggestions
- Add dashboard with a list of recent projects, files, and the following commands: new file, open file, open folder, edit config, quit
- Keybinds should be assigned based on functionlity and use frequency. Don't necessarily follow mnemonics for a keybind if another set of keybinds makes more sense. For example, S is used for search, not split. So the split keybinds should be in a different area, like the base layer of <leader>. If it's a common keybind that is used frequently, think about keeping it on the base layer. If it can be categorized, do that. Make sure that many of the most common use cases are covered.
- Integrate none-ls
- For Python, integrate Ruff, uv, and venv support.