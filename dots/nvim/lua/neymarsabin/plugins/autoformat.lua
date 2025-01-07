return {
	"stevearc/conform.nvim",
	opts = {
		notify_on_error = false,
		format_on_save = {
			timeout_ms = 2500,
			lsp_fallback = "fallback",
		},
		formatters_by_ft = {
			lua = { "stylua" },
			javascript = { { "prettier", "prettierd" } },
			typescript = { { "prettier", "prettierd" } },
			typescriptreact = { { "prettier", "prettierd" } },
			javascriptreact = { { "prettierd", "prettier" } },
			go = { { "gofmt" }, { "goimports" } },
		},
	},
}
