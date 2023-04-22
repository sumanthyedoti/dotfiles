local status_ok, bufferline = pcall(require, "bufferline")
if not status_ok then
	return
end

vim.opt.termguicolors = true
require("bufferline").setup({
	options = {
		offsets = { { filetype = "NvimTree", text = "", padding = 1 } },
		max_name_length = 30,
		max_prefix_length = 30, -- prefix used when a buffer is de-duplicated
		tab_size = 24,
		left_trunc_marker = "",
		right_trunc_marker = "",
	},
})
