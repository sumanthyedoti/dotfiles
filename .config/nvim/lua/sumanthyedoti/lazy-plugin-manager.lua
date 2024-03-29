-- Automatically install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  PACKMAN_BOOTSTRAP = vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
  print("Installing Lazy.nvim, close and reopen Neovim...")
end

vim.opt.rtp:prepend(lazypath)

if PACKMAN_BOOTSTRAP then
  require("lazy").sync()
end

-- Use a protected call so we don't error out on first use
local status_ok, lazy = pcall(require, "lazy")
if not status_ok then
  return
end

--########################################
-- Install your plugins here
--  packages are stored in '~/.local/share/nvim/lazy'
--[[ PACKAGES
  dispatch
  neo-minimap  - https://youtu.be/vNyQBWfSh7c
  neoclip
  colortils
    color-picker.nvim
  syntax-tree-surfer
  andweeb/presence.nvim (Discord)
  Wansmer/treesj
  dail.nvim
  doom.nvim
  anuvyklack/windows.nvim
  pretty-fold.nvim
  text-case.nvim
  fold-preview.nvim
  cphealper
    CompetiTest.nvim
  alpha-nvim
    neovim-session-manager
  telescope-file-browser
  vim-maximizer
  vim-illuminate
  vim-clap
  prettier.nvim
  hologram.nvim
  sleuth.vim
  nvim-notify
  overseer.nvim
  GitHub Copilot
  git-conflict.nvim
  neoformat
  nvim-peekup
  nabla.nvim
  Rnvimr
  Markdown:
  - vim-markdown
  - vim-markdown-toc
  - Preview:
    - glow
    - markdown-preview
  Writing:
  - goyo.vim
  - true-zen.nvim
  - zen-mode.nvim
  Note taking:
  - vimwiki
  - todo.txt-vim
  - vim-dotoo
  - Taskwarrior + VimWiki + TaskWiki
  - telekasten.nvim
  michaelb/sniprun *
  codi.vim
]]

local options = {
  install = { missing = true, colorscheme = { COLORSCHEME } },
  checker = { enabled = true, notify = false },
  change_detection = {
    enabled = true,
    notify = false,
  },
  ui = {
    -- a number <1 is a percentage., >1 is a fixed size
    size = { width = 0.8, height = 0.8 },
    wrap = true, -- wrap the lines in the ui
    -- The border to use for the UI window. Accepts same border values as |nvim_open_win()|.
    border = "none",
    icons = {
      cmd = " ",
      config = "",
      event = "",
      ft = " ",
      init = " ",
      import = " ",
      keys = " ",
      lazy = "∾ ",
      loaded = "●",
      not_loaded = "○",
      plugin = " ",
      runtime = " ",
      source = " ",
      start = "",
      task = "✔ ",
      list = {
        "●",
        "➜",
        "★",
        "‒",
      },
    },
  },
}

lazy.setup({
  { import = "sumanthyedoti.plugins" },
}, options)
