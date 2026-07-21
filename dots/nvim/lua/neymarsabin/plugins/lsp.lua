return {
	"neovim/nvim-lspconfig",
	dependencies = {
		-- Automatically install LSPs and related tools to stdpath for neovim
		"williamboman/mason.nvim",
		"williamboman/mason-lspconfig.nvim",
		"WhoIsSethDaniel/mason-tool-installer.nvim",

		-- Useful status updates for LSP.
		-- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
		{ "j-hui/fidget.nvim", opts = {} },
	},
	config = function()
		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("kickstart-lsp-attach", { clear = true }),
			callback = function(event)
				local map = function(keys, func, desc)
					vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
				end

				map("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")
				map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
				map("gi", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")
				map("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type [D]efinition")
				map("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")
				map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
				map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
				map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
				map("K", vim.lsp.buf.hover, "Hover Documentation")
				map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
				map("<leader>pd", function()
					require("telescope.builtin").lsp_definitions({ jump_type = "never" })
				end, "[P]eek [D]efinition")
				map("<C-s>", vim.lsp.buf.signature_help, "Signature Help")
				map("<leader>ge", vim.diagnostic.open_float, "Popup Errors")

				local client = vim.lsp.get_client_by_id(event.data.client_id)
				if client and client.server_capabilities.documentHighlightProvider then
					vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
						buffer = event.buf,
						callback = vim.lsp.buf.document_highlight,
					})

					vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
						buffer = event.buf,
						callback = vim.lsp.buf.clear_references,
					})
				end
			end,
		})

		local capabilities = vim.lsp.protocol.make_client_capabilities()
		capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())
		-- macOS: nvim's per-glob FSEvents/kqueue watchers exhaust fds in
		-- node_modules-sized repos (EMFILE in vim/_watch.lua); servers like
		-- ts_ls have their own internal file watching, so opt out entirely
		capabilities.workspace.didChangeWatchedFiles = { dynamicRegistration = false }

		-- catch-all for clients that don't go through vim.lsp.config at all
		-- (copilot.vim spawns its own client) or whose nvim-lspconfig defaults
		-- force-enable watching (tailwindcss): nvim checks this field on the
		-- live client at registration time, so stripping it here blocks the
		-- watcher regardless of how the client was started
		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("no-lsp-file-watchers", { clear = true }),
			callback = function(event)
				local client = vim.lsp.get_client_by_id(event.data.client_id)
				if client then
					client.capabilities = vim.tbl_deep_extend("force", client.capabilities or {}, {
						workspace = { didChangeWatchedFiles = { dynamicRegistration = false } },
					})
				end
			end,
		})

		local servers = {
			ts_ls = {
				filetypes = {
					"javascript",
					"javascriptreact",
					"javascript.jsx",
					"typescript",
					"typescriptreact",
					"typescript.tsx",
				},
			},
			bashls = {},
			gopls = {},
			eslint = {},
			dockerls = {},
			rust_analyzer = {},
			html = {},
			-- marksman removed: bundled .NET CoreCLR fails on this macOS (exit 137).
			-- markdown still highlights via treesitter. Re-add if .NET runtime fixed.
			docker_compose_language_service = {},
			terraformls = {},
			solargraph = {},
			pyright = {
				before_init = function(_, config)
					local path = require("lspconfig.util").path
					local cwd = config.root_dir or vim.fn.getcwd()
					-- check for common venv locations
					local venvs = {
						path.join(cwd, ".venv"),
						path.join(cwd, "venv"),
						path.join(cwd, ".env"),
						path.join(cwd, "env"),
					}
					for _, venv in ipairs(venvs) do
						if vim.fn.isdirectory(venv) == 1 then
							config.settings = vim.tbl_deep_extend("force", config.settings or {}, {
								python = {
									pythonPath = path.join(venv, "bin", "python"),
								},
							})
							return
						end
					end
					-- fallback: check VIRTUAL_ENV env var
					local virtual_env = os.getenv("VIRTUAL_ENV")
					if virtual_env then
						config.settings = vim.tbl_deep_extend("force", config.settings or {}, {
							python = {
								pythonPath = path.join(virtual_env, "bin", "python"),
							},
						})
					end
				end,
				settings = {
					python = {
						analysis = {
							autoSearchPaths = true,
							useLibraryCodeForTypes = true,
							diagnosticMode = "openFilesOnly",
						},
					},
				},
			},
			lua_ls = {
				settings = {
					Lua = {
						runtime = { version = "LuaJIT" },
						workspace = {
							checkThirdParty = false,
							-- Tells lua_ls where to find all the Lua files that you have loaded
							-- for your neovim configuration.
							library = {
								"${3rd}/luv/library",
								unpack(vim.api.nvim_get_runtime_file("", true)),
							},
							-- If lua_ls is really slow on your computer, you can try this instead:
							-- library = { vim.env.VIMRUNTIME },
						},
						-- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
						-- diagnostics = { disable = { 'missing-fields' } },
					},
				},
			},
		}

		--  You can press `g?` for help in this menu
		require("mason").setup()

		-- You can add other tools here that you want Mason to install
		-- for you, so that they are available from within Neovim.
		local ensure_installed = vim.tbl_keys(servers or {})
		vim.list_extend(ensure_installed, {
			"stylua", -- Used to format lua code
			"ruff", -- Python formatter and linter
		})
		require("mason-tool-installer").setup({ ensure_installed = ensure_installed })

		-- mason-lspconfig v2 removed `handlers` and auto-enables installed
		-- servers via vim.lsp.enable(); configure through vim.lsp.config instead.
		-- "*" merges into every server — this is what actually carries the
		-- didChangeWatchedFiles opt-out (EMFILE fix) to running servers.
		vim.lsp.config("*", { capabilities = capabilities })
		-- nvim-lspconfig's tailwindcss default force-enables file watching,
		-- which beats the "*" opt-out above — override it explicitly
		vim.lsp.config("tailwindcss", {
			capabilities = { workspace = { didChangeWatchedFiles = { dynamicRegistration = false } } },
		})
		for server_name, server in pairs(servers) do
			vim.lsp.config(server_name, {
				cmd = server.cmd,
				settings = server.settings,
				filetypes = server.filetypes,
				capabilities = server.capabilities,
			})
		end

		require("mason-lspconfig").setup()
	end,
}
