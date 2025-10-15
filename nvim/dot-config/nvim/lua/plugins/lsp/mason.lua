return {
  "williamboman/mason.nvim",
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",
  },
  config = function()
    require("mason").setup({
      ui = {
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗",
        },
      },
    })

    require("mason-lspconfig").setup({
      ensure_installed = {
        "lua_ls",
        "pylsp", -- Python LSP (replaces pyright)
        "gopls",
        "terraformls",
        "ansiblels",
        "jsonls",
        "yamlls",
        "bashls",
      },
      automatic_installation = false, -- Disable to prevent unwanted auto-installs
    })

    require("mason-tool-installer").setup({
      ensure_installed = {
        -- Lua
        "stylua",

        -- Python
        "ruff", -- Linter/formatter

        -- Go
        "gofumpt",
        "goimports",
        "golangci-lint",

        -- JavaScript/TypeScript
        "prettier",
        "eslint_d",

        -- Shell
        "shfmt",
        "shellcheck",

        -- Terraform
        "terraform-ls",
        "tflint",

        -- Ansible
        "ansible-lint",

        -- JSON/YAML
        "jsonlint",
        "yamllint",

        -- Markdown
        "markdownlint",

        -- Spell checking
        "codespell",
      },
    })
  end,
}
