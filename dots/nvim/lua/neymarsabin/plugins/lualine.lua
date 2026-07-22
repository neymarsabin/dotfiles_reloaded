local function home()
	return "zerocool"
end

local function python_venv()
	local venv = os.getenv("VIRTUAL_ENV") or os.getenv("CONDA_DEFAULT_ENV")
	if venv then
		return " " .. vim.fn.fnamemodify(venv, ":t")
	end
	local cwd = vim.fn.getcwd()
	for _, name in ipairs({ ".venv", "venv" }) do
		if vim.fn.isdirectory(cwd .. "/" .. name) == 1 then
			return " " .. name
		end
	end
	return ""
end

-- names of LSP clients attached to the current buffer, e.g. "  pyright"
local function lsp_clients()
	local buf = vim.api.nvim_get_current_buf()
	local names = {}
	for _, client in ipairs(vim.lsp.get_clients({ bufnr = buf })) do
		if client.name ~= "null-ls" then
			table.insert(names, client.name)
		end
	end
	if #names == 0 then
		return ""
	end
	return "  " .. table.concat(names, ",")
end

return {
	"nvim-lualine/lualine.nvim",
	dependencies = { "nvim-tree/nvim-web-devicons" },
	config = function()
		require("lualine").setup({
			options = {
				icons_enabled = true,
				theme = "auto", -- follows whatever colorscheme is active
				component_separators = { left = "", right = "" },
				section_separators = { left = "", right = "" },
				disabled_filetypes = {
					statusline = { "neo-tree", "neogit", "Outline" },
				},
				always_divide_middle = true,
				globalstatus = true, -- one statusline for all splits (cleaner)
				refresh = { statusline = 300 },
			},
			sections = {
				lualine_a = {
					{ "mode", separator = { left = "" }, right_padding = 2 },
				},
				lualine_b = {
					{ "branch", icon = "" },
					{
						"diff",
						symbols = { added = " ", modified = " ", removed = " " },
					},
				},
				lualine_c = {
					{ "filename", path = 1, symbols = { modified = "  ", readonly = "  " } },
					{
						-- macro recording indicator
						function()
							local rec = vim.fn.reg_recording()
							return rec ~= "" and ("  recording @" .. rec) or ""
						end,
						color = { fg = "#ff9e64" },
					},
				},
				lualine_x = {
					{
						"diagnostics",
						sources = { "nvim_lsp" },
						symbols = { error = " ", warn = " ", info = " ", hint = " " },
					},
					{ lsp_clients, color = { fg = "#7aa2f7" } },
					{ python_venv, color = { fg = "#9ece6a" }, cond = function()
						return vim.bo.filetype == "python"
					end },
					"searchcount",
				},
				lualine_y = {
					{ "filetype", colored = true, icon_only = false },
					"encoding",
					"fileformat",
					"progress",
				},
				lualine_z = {
					{ home, separator = { right = "" } },
					{ "location", separator = { right = "" }, left_padding = 2 },
				},
			},
			inactive_sections = {
				lualine_c = { { "filename", path = 1 } },
				lualine_x = { "location" },
			},
			extensions = { "neo-tree", "trouble", "lazy", "mason", "fugitive", "quickfix" },
		})
	end,
}
