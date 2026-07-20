-- nvim-treesitter-textobjects `main` branch (matches nvim-treesitter main).
-- main removed `.configs.setup` modules; keymaps are now wired manually against
-- the select/move/swap submodules.
return {
	"nvim-treesitter/nvim-treesitter-textobjects",
	branch = "main",
	dependencies = { "nvim-treesitter/nvim-treesitter" },
	event = { "BufReadPre", "BufNewFile" },
	config = function()
		require("nvim-treesitter-textobjects").setup({
			select = { lookahead = true },
			move = { set_jumps = true },
		})

		local select = require("nvim-treesitter-textobjects.select")
		local move = require("nvim-treesitter-textobjects.move")
		local swap = require("nvim-treesitter-textobjects.swap")

		-- select
		local sel = {
			af = { "@function.outer", "Around function" },
			["if"] = { "@function.inner", "Inside function" },
			ac = { "@class.outer", "Around class" },
			ic = { "@class.inner", "Inside class" },
			aa = { "@parameter.outer", "Around argument" },
			ia = { "@parameter.inner", "Inside argument" },
			ai = { "@conditional.outer", "Around if" },
			ii = { "@conditional.inner", "Inside if" },
			al = { "@loop.outer", "Around loop" },
			il = { "@loop.inner", "Inside loop" },
		}
		for lhs, spec in pairs(sel) do
			vim.keymap.set({ "x", "o" }, lhs, function()
				select.select_textobject(spec[1], "textobjects")
			end, { desc = spec[2] })
		end

		-- move
		local function m(fn, query)
			return function()
				fn(query, "textobjects")
			end
		end
		vim.keymap.set({ "n", "x", "o" }, "]m", m(move.goto_next_start, "@function.outer"), { desc = "Next method start" })
		vim.keymap.set({ "n", "x", "o" }, "]c", m(move.goto_next_start, "@class.outer"), { desc = "Next class start" })
		vim.keymap.set({ "n", "x", "o" }, "]M", m(move.goto_next_end, "@function.outer"), { desc = "Next method end" })
		vim.keymap.set({ "n", "x", "o" }, "]C", m(move.goto_next_end, "@class.outer"), { desc = "Next class end" })
		vim.keymap.set({ "n", "x", "o" }, "[m", m(move.goto_previous_start, "@function.outer"), { desc = "Prev method start" })
		vim.keymap.set({ "n", "x", "o" }, "[c", m(move.goto_previous_start, "@class.outer"), { desc = "Prev class start" })
		vim.keymap.set({ "n", "x", "o" }, "[M", m(move.goto_previous_end, "@function.outer"), { desc = "Prev method end" })
		vim.keymap.set({ "n", "x", "o" }, "[C", m(move.goto_previous_end, "@class.outer"), { desc = "Prev class end" })

		-- swap
		vim.keymap.set("n", "<leader>sa", function()
			swap.swap_next("@parameter.inner")
		end, { desc = "Swap with next arg" })
		vim.keymap.set("n", "<leader>sA", function()
			swap.swap_previous("@parameter.inner")
		end, { desc = "Swap with prev arg" })
	end,
}
