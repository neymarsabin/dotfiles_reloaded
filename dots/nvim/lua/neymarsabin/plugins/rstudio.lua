local function setup_r_format_on_save()
	vim.api.nvim_create_augroup("RFormatOnSave", { clear = true })
	vim.api.nvim_create_autocmd("BufWritePre", {
		group = "RFormatOnSave",
		pattern = "*.R",
		callback = function()
			-- Check if the Rformat command is available
			if vim.fn.exists(":Rformat") == 2 then
				local cursor_pos = vim.api.nvim_win_get_cursor(0)
				-- Run the Rformat command
				vim.cmd("Rformat")
				-- Restore the cursor position
				vim.api.nvim_win_set_cursor(0, cursor_pos)
			else
				vim.notify("Rformat command not found. Please ensure it's available.", vim.log.levels.WARN)
			end
		end,
	})
end

return {
	{
		"jamespeapen/Nvim-R",
		lazy = false,
		version = "~0.1.0",
		config = function()
			setup_r_format_on_save()
		end,
	},
}
