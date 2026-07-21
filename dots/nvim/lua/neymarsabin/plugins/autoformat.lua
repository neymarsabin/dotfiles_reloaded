-- return some other r conform
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
			javascript = { "prettier", "prettierd", stop_after_first = true },
			typescript = { "prettier", "prettierd", stop_after_first = true },
			typescriptreact = { "prettier", "prettierd", stop_after_first = true },
			javascriptreact = { "prettier", "prettierd", stop_after_first = true },
			go = { "gofmt", "goimports", stop_after_first = false },
			python = { "ruff_format", "ruff_fix" },
		},
	},
}
