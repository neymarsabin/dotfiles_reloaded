-- UI utilities: a theme switcher that persists your choice, plus quick toggles.
-- Not a plugin per se — returns an empty spec and wires keymaps on VeryLazy.

return {
	"folke/which-key.nvim", -- piggyback an already-present plugin so this file loads
	optional = true,
	init = function()
		-- persist the active colorscheme whenever it changes, so it survives restart
		vim.api.nvim_create_autocmd("ColorScheme", {
			callback = function(args)
				local path = _G.ThemePersistPath or (vim.fn.stdpath("data") .. "/theme.txt")
				local f = io.open(path, "w")
				if f then
					f:write(args.match)
					f:close()
				end
			end,
		})

		local map = vim.keymap.set

		-- ── theme switcher ──────────────────────────────────────────────────────
		-- telescope previews each scheme live as you move through the list, and
		-- the ColorScheme autocmd above persists whichever one you land on
		map("n", "<leader>ut", function()
			-- lazy themes are not on the runtimepath until something loads them,
			-- so the picker would only list the handful already in use; pull in
			-- every plugin that ships a colors/ dir before opening it
			for name, spec in pairs(require("lazy.core.config").plugins) do
				if spec.dir and vim.fn.isdirectory(spec.dir .. "/colors") == 1 then
					pcall(require("lazy").load, { plugins = { name } })
				end
			end

			require("telescope.builtin").colorscheme({
				enable_preview = true,
				ignore_builtins = true,
			})
		end, { desc = "Switch theme (live preview, persists)" })

		-- ── toggles ───────────────────────────────────────────────────────────
		map("n", "<leader>uw", function()
			vim.opt.wrap = not vim.opt.wrap:get()
			vim.notify("wrap = " .. tostring(vim.opt.wrap:get()))
		end, { desc = "Toggle wrap" })

		map("n", "<leader>ur", function()
			vim.opt.relativenumber = not vim.opt.relativenumber:get()
			vim.notify("relativenumber = " .. tostring(vim.opt.relativenumber:get()))
		end, { desc = "Toggle relative number" })

		map("n", "<leader>us", function()
			vim.opt.spell = not vim.opt.spell:get()
			vim.notify("spell = " .. tostring(vim.opt.spell:get()))
		end, { desc = "Toggle spell check" })

		local diag_on = true
		map("n", "<leader>ud", function()
			diag_on = not diag_on
			if diag_on then
				vim.diagnostic.enable()
			else
				vim.diagnostic.enable(false)
			end
			vim.notify("diagnostics = " .. tostring(diag_on))
		end, { desc = "Toggle diagnostics" })

		map("n", "<leader>uc", function()
			local col = vim.opt.colorcolumn:get()
			vim.opt.colorcolumn = (#col == 0) and "80" or ""
			vim.notify("colorcolumn = " .. (vim.opt.colorcolumn:get()[1] or "off"))
		end, { desc = "Toggle color column" })
	end,
}
