return {
  "nvimtools/none-ls.nvim",
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvimtools/none-ls-extras.nvim",
  },
  config = function()
    local null_ls = require("null-ls")
    local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

    -- Helpers
    local formatting = null_ls.builtins.formatting
    local diagnostics = null_ls.builtins.diagnostics
    local code_actions = null_ls.builtins.code_actions

    null_ls.setup({
      sources = {
        -- Lua
        formatting.stylua,

        -- Python
        require("none-ls.formatting.ruff"), -- Ruff for formatting (replaces black)
        require("none-ls.diagnostics.ruff"), -- Ruff for linting (replaces pylint, isort, flake8)

        -- Go
        formatting.gofumpt,
        formatting.goimports,
        diagnostics.golangci_lint,

        -- JavaScript/TypeScript
        formatting.prettier.with({
          extra_filetypes = { "svelte" },
        }),
        require("none-ls.diagnostics.eslint_d"),
        require("none-ls.code_actions.eslint_d"),

        -- JSON/YAML
        diagnostics.jsonlint,
        diagnostics.yamllint,

        -- Shell
        formatting.shfmt,
        diagnostics.shellcheck,
        code_actions.shellcheck,

        -- Terraform
        formatting.terraform_fmt,
        diagnostics.terraform_validate,
        diagnostics.tflint,

        -- Ansible
        diagnostics.ansiblelint,

        -- Markdown
        formatting.prettier,
        diagnostics.markdownlint,

        -- Git
        code_actions.gitsigns,

        -- Spell checking
        diagnostics.codespell,
      },

      -- Format on save
      on_attach = function(client, bufnr)
        if client:supports_method("textDocument/formatting") then
          vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
          vim.api.nvim_create_autocmd("BufWritePre", {
            group = augroup,
            buffer = bufnr,
            callback = function()
              vim.lsp.buf.format({
                bufnr = bufnr,
                filter = function(c)
                  return c.name == "null-ls"
                end,
              })
            end,
          })
        end
      end,
    })
  end,
}
