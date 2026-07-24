return {
	"nvim-telescope/telescope.nvim",
	-- master, not 0.1.x: old branch calls nvim-treesitter's removed ft_to_lang()
	-- in previewers and errors on the treesitter `main` branch
	branch = "master",
	priority = 10000000,
	dependencies = {
		"nvim-lua/plenary.nvim",
		{
			"nvim-telescope/telescope-fzf-native.nvim",
			build = "make",
			cond = function()
				return vim.fn.executable("make") == 1
			end,
		},
		{ "nvim-telescope/telescope-ui-select.nvim" },
		{ "nvim-tree/nvim-web-devicons" },
	},
	config = function()
		require("telescope").setup({
			defaults = {
				layout_strategy = "bottom_pane",
				layout_config = {
					bottom_pane = {
						height = 15,
						preview_cutoff = 80,
						prompt_position = "bottom",
					},
				},
				path_display = { "truncate" },
				previewer = true,
				file_previewer = require("telescope.previewers").vim_buffer_cat.new,
				grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
				qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
				mappings = {
					n = {
						["<C-g>"] = require("telescope.actions").close,
					},
				},
			},
			extensions = {
				["ui-select"] = {
					require("telescope.themes").get_dropdown(),
				},
			},
		})

		-- Enable telescope extensions, if they are installed
		pcall(require("telescope").load_extension, "fzf")
		pcall(require("telescope").load_extension, "ui-select")
		pcall(require("telescope").load_extension, "harpoon")

		-- See `:help telescope.builtin`
		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader>hf", builtin.help_tags, {})
		vim.keymap.set("n", "<leader>hk", builtin.keymaps, {})
		vim.keymap.set("n", "<leader>pf", builtin.find_files, {})
		vim.keymap.set("n", "<leader>pg", builtin.git_files, {})
		vim.keymap.set("n", "<leader>p/", builtin.live_grep, {})
		vim.keymap.set("n", "<leader>ds", builtin.diagnostics, {})
		vim.keymap.set("n", "<leader>sr", builtin.resume, {})
		vim.keymap.set("n", "<leader>bb", builtin.buffers, {})

		-- use telescope to find commands
		vim.keymap.set("n", "<leader>cc", builtin.commands, {})

		-- Slightly advanced example of overriding default behavior and theme
		vim.keymap.set("n", "<leader>ss", function()
			-- You can pass additional configuration to telescope to change theme, layout, etc.
			builtin.current_buffer_fuzzy_find({})
		end, { desc = "Fuzzily search in current buffer" })

		-- Also possible to pass additional configuration options.
		--  See `:help telescope.builtin.live_grep()` for information about particular keys
		vim.keymap.set("n", "<leader>s/", function()
			builtin.live_grep({
				grep_open_files = true,
				prompt_title = "Live Grep in Open Files",
			})
		end, { desc = "Live grep in open files" })

		-- Shortcut for searching your neovim configuration files
		vim.keymap.set("n", "<leader>sn", function()
			builtin.find_files({ cwd = vim.fn.stdpath("config") })
		end, { desc = "[S]earch [N]eovim files" })

		-- open harpoon with telescope search enabled
		vim.keymap.set("n", "<leader>mm", ":Telescope harpoon marks <CR>", { desc = "[S]earch [N]eovim files" })
	end,
}
