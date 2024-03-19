return {
	{
		"stevearc/conform.nvim",
		opts = {},
		config = function()
			local conform = require("conform")

			conform.setup({
				formatters_by_ft = {
					lua = { "stylua" },
					javascript = { { "prettierd", "prettier" } },
					yaml = { { "prettierd", "prettier" } },
				},
				format_on_save = {
					timeout_ms = 500,
					lsp_fallback = true,
					async = false,
				},
			})
		end,
	},
}
