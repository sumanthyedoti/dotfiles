return {
	"ziontee113/icon-picker.nvim",
	event = "BufEnter",
	config = function()
		require("icon-picker").setup({
			disable_legacy_commands = true,
		})
	end,
	dependencies = {
		"stevearc/dressing.nvim",
	},
}
