local colorscheme = "tokyonight-night"

require('lualine').setup {
  options = { theme = 'gruvbox' },
}

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
  vim.notify("colorscheme " .. colorscheme .. " not found!")
  return
end
