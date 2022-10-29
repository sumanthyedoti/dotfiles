local status_ok, glow = pcall(require, "glow")
if not status_ok then
	return
end

glow.setup({
	border = "shadow", -- floating window border config
	style = "dark", -- filled automatically with your current editor background, you can override using glow json style
	pager = false,
	width = 80,
})
