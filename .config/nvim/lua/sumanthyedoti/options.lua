-- :help options
local o = vim.opt
o.backup = false -- creates a backup file
o.clipboard = "unnamedplus" -- allows neovim to access the system clipboard
o.ignorecase = true -- ignore case in search patterns
o.lazyredraw = true -- does not redraw for macros
o.cmdheight = 2 -- more space in the neovim command line for displaying messages
o.completeopt = { "menuone", "noselect" } -- mostly just for cmp
o.conceallevel = 0 -- so that `` is visible in markdown files
o.fileencoding = "utf-8" -- the encoding written to a file
o.hlsearch = true -- highlight all matches on previous search pattern
o.incsearch = true
o.foldcolumn = "2"
o.smartcase = true
o.backspace = "indent,eol,start"
o.title = true
o.mouse = "a" -- allow the mouse to be used in neovim
o.pumheight = 10 -- pop up menu height
o.gp = "git grep -n"
o.showmode = true -- NORMAL | INSERT | VISUAL
o.nrformats = { "bin", "octal", "hex", "alpha" } -- alpha to increment alphabets
o.showtabline = 2 -- always show tabs
o.smartindent = true -- make indenting smarter
o.splitbelow = true -- force all horizontal splits to go below current window
o.splitright = true -- force all vertical splits to go to the right of current window
o.swapfile = false -- creates a swapfile
o.termguicolors = true -- set term gui colors (most terminals support this)
o.timeoutlen = 1000 -- time to wait for a mapped sequence to complete (in milliseconds)
o.undofile = true -- enable persistent undo
o.updatetime = 500 -- faster completion (4000ms default)
o.writebackup = false -- if a file is being edited by another program, it is not allowed to be edited
o.guifont = "JetBrainsMono Nerd Font:h17" -- the font used in graphical neovim applications
o.shortmess:append("c")
o.path:append({ "**" }) -- find files searching into subfolders
o.wildoptions = "pum"
o.pumblend = 10
o.winblend = 10
o.wildignore:append({ "*/node_modules/*" }) -- find files searching into subfolders
o.background = "dark"
---- setting options with for loop by iteratings a k-v pair in a table
local options = {
  expandtab = true, -- convert tabs to spaces
  shiftwidth = 2, -- the number of spaces inserted for each indentation
  tabstop = 2, -- insert 2 spaces for a tab
  softtabstop = 2,
  cursorline = true, -- highlight the current line
  number = true, -- set numbered lines
  textwidth = 80,
  relativenumber = true, -- set relative numbered lines
  numberwidth = 2, -- set number column width to 2 {default 4}
  signcolumn = "yes", -- always show the sign column, otherwise it would shift the text each time
  wrap = false, -- display lines as one long line
  linebreak = true,
  scrolloff = 4, -- number of screen lines to keep above and below the cursor
  sidescrolloff = 12,
}
for k, v in pairs(options) do
  vim.opt[k] = v
end

-- vim.cmd to pass vim-script as string
local vs = vim.cmd
vs("set whichwrap+=<,>,[,],h,l")

-- Undercurl
vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])
