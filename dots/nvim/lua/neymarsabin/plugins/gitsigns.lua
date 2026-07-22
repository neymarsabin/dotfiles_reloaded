return {
	{
		"lewis6991/gitsigns.nvim",
		event = { "BufReadPre", "BufNewFile" },
		opts = {
			signs = {
				add = { text = "▎" },
				change = { text = "▎" },
				delete = { text = "" },
				topdelete = { text = "" },
				changedelete = { text = "▎" },
				untracked = { text = "▎" },
			},
			signs_staged = {
				add = { text = "▎" },
				change = { text = "▎" },
				delete = { text = "" },
				topdelete = { text = "" },
				changedelete = { text = "▎" },
			},
			preview_config = {
				border = "rounded",
			},
			on_attach = function(bufnr)
				local gs = package.loaded.gitsigns

				local function map(mode, l, r, desc)
					vim.keymap.set(mode, l, r, { buffer = bufnr, desc = desc })
				end

				-- navigate hunks
				map("n", "]h", function()
					if vim.wo.diff then
						vim.cmd.normal({ "]c", bang = true })
					else
						gs.nav_hunk("next")
					end
				end, "Next hunk")
				map("n", "[h", function()
					if vim.wo.diff then
						vim.cmd.normal({ "[c", bang = true })
					else
						gs.nav_hunk("prev")
					end
				end, "Prev hunk")

				-- accept / reject
				map("n", "<leader>hs", gs.stage_hunk, "Stage hunk (accept)")
				map("v", "<leader>hs", function()
					gs.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
				end, "Stage hunk (accept)")
				map("n", "<leader>hr", gs.reset_hunk, "Reset hunk (reject)")
				map("v", "<leader>hr", function()
					gs.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
				end, "Reset hunk (reject)")
				map("n", "<leader>hS", gs.stage_buffer, "Stage buffer (accept all)")
				map("n", "<leader>hR", gs.reset_buffer, "Reset buffer (reject all)")
				map("n", "<leader>hu", gs.undo_stage_hunk, "Undo stage hunk")

				-- preview inline (like Cursor's diff view)
				map("n", "<leader>hp", gs.preview_hunk_inline, "Preview hunk inline")
				map("n", "<leader>hP", gs.preview_hunk, "Preview hunk (float)")

				-- blame
				map("n", "<leader>hb", function()
					gs.blame_line({ full = true })
				end, "Blame line")
				map("n", "<leader>hd", gs.diffthis, "Diff this")
			end,
		},
	},
}
