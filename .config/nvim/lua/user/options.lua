-- :help options
local o = vim.opt
o.backup = false                          -- creates a backup file
o.clipboard = "unnamedplus"               -- allows neovim to access the system clipboard
o.cmdheight = 2                           -- more space in the neovim command line for displaying messages
o.completeopt = { "menuone", "noselect" } -- mostly just for cmp
o.conceallevel = 0                        -- so that `` is visible in markdown files
o.fileencoding = "utf-8"                  -- the encoding written to a file
o.hlsearch = true                         -- highlight all matches on previous search pattern
o.ignorecase = true                       -- ignore case in search patterns
o.mouse = "a" --                             -- allow the mouse to be used in neovim
o.pumheight = 10                          -- pop up menu height
-- o.showmode = false                        -- we don't need to see things like -- INSERT -- anymore
o.showtabline = 2                         -- always show tabs
o.smartcase = true                        -- smart case
o.smartindent = true                      -- make indenting smarter
o.splitbelow = true                       -- force all horizontal splits to go below current window
o.splitright = true                       -- force all vertical splits to go to the right of current window
o.swapfile = false                        -- creates a swapfile
-- o.termguicolors = true                    -- set term gui colors (most terminals support this)
o.timeoutlen = 1000                       -- time to wait for a mapped sequence to complete (in milliseconds)
o.undofile = true                         -- enable persistent undo
o.updatetime = 500                        -- faster completion (4000ms default)
o.writebackup = false                     -- if a file is being edited by another program, it is not allowed to be edited
o.guifont = "monospace:h17"               -- the font used in graphical neovim applications
o.shortmess:append "c"
---- setting options with for loop by iteratings a k-v pair in a table
local options = {
  expandtab = true,                        -- convert tabs to spaces
  shiftwidth = 2,                          -- the number of spaces inserted for each indentation
  tabstop = 2,                             -- insert 2 spaces for a tab
  cursorline = true,                       -- highlight the current line
  number = true,                           -- set numbered lines
  relativenumber = true,                  -- set relative numbered lines
  numberwidth = 2,                         -- set number column width to 2 {default 4}
  signcolumn = "yes",                      -- always show the sign column, otherwise it would shift the text each time
  wrap = false,                            -- display lines as one long line
  scrolloff = 6,                           -- number of screen lines to keep above and below the cursor
  sidescrolloff = 4,
}
for k, v in pairs(options) do
  vim.opt[k] = v
end

-- vim.cmd to pass vim-script as string
vs = vim.cmd
vs "set whichwrap+=<,>,[,],h,l"
vs [[set iskeyword+=-]]
