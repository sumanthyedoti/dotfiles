require("user.options")
require("user.keymaps")
require("user.plugins")
require("user.colorscheme")
require("user.cmp")
require("user.lsp")
require("user.telescope")
require("user.dressing")
require("user.treesitter")
require("user.nvim-tree")
require("user.bufferline")
require('lualine').setup {
  options = { theme  = 'gruvbox' },
}

-- Highlight on Yank
vim.cmd [[
  au TextYankPost * silent! lua vim.highlight.on_yank()
  au TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=250}
]]
-- Delete without yanking if preperded by <leader>
vim.cmd [[nnoremap <leader>dd "_dd]]

-- Trim trailing whitespace
vim.cmd [[
  fun! TrimWhitespace()
      let l:save = winsaveview()
      keeppatterns %s/\s\+$//e
      call winrestview(l:save)
  endfun
  au BufWritePre * :call TrimWhitespace()
]]
