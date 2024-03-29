return {
	{
		"akinsho/org-bullets.nvim",
		config = function()
			require("org-bullets").setup({
				concealcursor = false,
				symbols = { "◉", "○", "✸", "✿" },
				list = "•",
				checkboxes = {
					half = { "-", "OrgTSCheckboxHalfChecked" },
					done = { "✓", "OrgDone" },
					todo = { "x", "OrgTODO" },
				},
			})
		end,
	},
	{
		"nvim-orgmode/orgmode",
		dependencies = {
			{ "nvim-treesitter/nvim-treesitter", lazy = true },
		},
		event = "VeryLazy",
		config = function()
			require("orgmode").setup_ts_grammar()
			require("orgmode").setup({
				org_agenda_files = "~/projects/pet/the-new-org/doom/**",
				org_default_notes_file = "~/projects/pet/the-new-org/refile.org",
				org_todo_keywords = {
					"TODO",
					"WAITING",
					"DOING",
					"BLOCKED",
					"|",
					"PAUSED",
					"DONE",
					"COMPLETED",
					"CANCELLED",
				},
				org_agenda_skip_scheduler_if_done = true,
				org_agenda_skip_deadline_if_done = true,
				org_agenda_span = "week",
				org_deadline_warning_days = 7,
				org_agenda_start_day = "-1d",
				win_split_mode = "horizontal",
				org_startup_folded = "overview",
				org_hide_leading_stars = true,
				org_startup_indented = true,
				org_adapt_indentation = true,
				org_edit_src_content_indentation = 2,
			})
		end,
	},
}
