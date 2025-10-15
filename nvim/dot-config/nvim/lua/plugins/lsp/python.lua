return {
  "linux-cultist/venv-selector.nvim",
  dependencies = {
    "neovim/nvim-lspconfig",
    "nvim-telescope/telescope.nvim",
    "mfussenegger/nvim-dap-python",
  },
  ft = "python",
  keys = {
    { "<leader>vs", "<cmd>VenvSelect<cr>", desc = "Select Python venv" },
    { "<leader>vc", "<cmd>VenvSelectCached<cr>", desc = "Select cached Python venv" },
  },
  config = function()
    require("venv-selector").setup({
      -- Auto select venv when entering a Python project
      auto_refresh = true,
      search_venv_managers = true,
      search_workspace = true,

      -- Support for different venv managers
      name = {
        "venv",
        ".venv",
        "env",
        ".env",
      },

      -- Support for uv
      fd_binary_name = "fd",
      dap_enabled = true,

      -- Notify when venv is selected
      notify_user_on_activate = true,

      -- Path to uv and other Python tools
      pipenv_path = "pipenv",
      poetry_path = "poetry",
      pyenv_path = "pyenv",
    })

    -- Auto-activate venv when opening Python files
    vim.api.nvim_create_autocmd("FileType", {
      pattern = "python",
      callback = function()
        -- Try to auto-select venv (pcall to handle if method doesn't exist)
        pcall(function()
          local venv = require("venv-selector")
          if venv.retrieve_from_cache then
            venv.retrieve_from_cache()
          end
        end)
      end,
    })
  end,
}
