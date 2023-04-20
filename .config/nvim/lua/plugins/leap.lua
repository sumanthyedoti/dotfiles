local status_ok, leap = pcall(require, "leap")
if not status_ok then
	return
end

leap.add_default_mappings()

leap.opts = {}

vim.cmd([[
  autocmd ColorScheme * lua require('leap').init_highlight(true)
]])
