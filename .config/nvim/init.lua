require("user.options")
require("user.keymaps")
require("user.plugins")
require("user.colorscheme")
require("user.cmp")
require("user.lsp")
require('lualine').setup {
  options = { theme  = 'gruvbox' },
}

-- Highlight on Yank
vim.cmd [[
  au TextYankPost * silent! lua vim.highlight.on_yank()
  au TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=250}
]]
