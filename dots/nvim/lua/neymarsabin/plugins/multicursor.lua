return {
	"mg979/vim-visual-multi",
	branch = "master",
	init = function()
		vim.g.VM_maps = {
			["Find Under"] = "<C-n>",
			["Find Subword Under"] = "<C-n>",
			["Select All"] = "<C-S-n>",
			["Skip Region"] = "q",
			["Remove Region"] = "Q",
			["Add Cursor Down"] = "<C-Down>",
			["Add Cursor Up"] = "<C-Up>",
		}
	end,
}
