return {
	"neymarsabin/fitness.nvim",
	dir = "~/projects/oss/fitness.nvim",

	config = function()
		require("fitness").setup({
			name = "sabin",
		})
	end,
}
