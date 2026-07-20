-- nvim-treesitter `main` branch (required for nvim 0.11+; `master` is archived and
-- crashes nvim 0.12's injection engine with "attempt to call method 'range'").
-- main branch removed the `.configs.setup` modules: highlighting is now native
-- (vim.treesitter.start), parsers are installed via the install() API.
return {
	"nvim-treesitter/nvim-treesitter",
	branch = "main",
	lazy = false,
	build = ":TSUpdate",
	config = function()
		local ensure = {
			"bash",
			"html",
			"lua",
			"go",
			"toml",
			"yaml",
			"json",
			"javascript",
			"typescript",
			"css",
			"scss",
			"tsx",
			"markdown",
			"markdown_inline", -- required for fenced code blocks in markdown
			"terraform",
			"python",
		}
		require("nvim-treesitter").install(ensure)

		-- enable treesitter highlighting + indentation per buffer
		vim.api.nvim_create_autocmd("FileType", {
			callback = function(args)
				local buf = args.buf
				local lang = vim.treesitter.language.get_lang(vim.bo[buf].filetype)
				if not lang then
					return
				end
				-- only start when a parser is actually available (skip silently otherwise)
				if not pcall(vim.treesitter.start, buf, lang) then
					return
				end
				vim.bo[buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
			end,
		})

		-- incremental selection (main branch dropped the builtin module; native reimpl)
		local stacks = setmetatable({}, { __mode = "k" })

		local function select_node(buf, node)
			local srow, scol, erow, ecol = node:range()
			vim.api.nvim_win_set_cursor(0, { srow + 1, scol })
			vim.cmd("normal! v")
			vim.api.nvim_win_set_cursor(0, { erow + 1, math.max(ecol - 1, 0) })
		end

		local function same_range(a, b)
			local a1, a2, a3, a4 = a:range()
			local b1, b2, b3, b4 = b:range()
			return a1 == b1 and a2 == b2 and a3 == b3 and a4 == b4
		end

		local function init_selection()
			local buf = vim.api.nvim_get_current_buf()
			local node = vim.treesitter.get_node()
			if not node then
				return
			end
			stacks[buf] = { node }
			select_node(buf, node)
		end

		local function node_incremental()
			local buf = vim.api.nvim_get_current_buf()
			local stack = stacks[buf]
			if not stack or #stack == 0 then
				return init_selection()
			end
			local node = stack[#stack]
			local parent = node:parent()
			while parent and same_range(parent, node) do
				parent = parent:parent()
			end
			if parent then
				stack[#stack + 1] = parent
				select_node(buf, parent)
			else
				select_node(buf, node)
			end
		end

		local function node_decremental()
			local buf = vim.api.nvim_get_current_buf()
			local stack = stacks[buf]
			if not stack or #stack <= 1 then
				return
			end
			stack[#stack] = nil
			select_node(buf, stack[#stack])
		end

		vim.keymap.set("n", "<CR>", init_selection, { desc = "TS init selection" })
		vim.keymap.set("x", "<CR>", node_incremental, { desc = "TS increment node" })
		vim.keymap.set("x", "<BS>", node_decremental, { desc = "TS decrement node" })
	end,
}
