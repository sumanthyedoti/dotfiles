-- following options are the default
-- each of these are documented in `:help nvim-tree.OPTION_NAME`

--[[
-- E - expand all
-- W - collapase
-- H - toggle .dotfiles
-- I - toggle gitignored
-- <C-x> - Horizontal split
-- <C-r> - Vertical split
-- <C-k> - Stats
-- a - create
-- x - cut
-- c - copy
-- p - paste
-- m - mark
-- R - refresh
-- bmv - bulk move marked
-- y - copy name
-- Y - copy relative path
-- r - rename
-- e - edit basename
-- o - open
--  - close directory
--]]

return {
  "nvim-tree/nvim-tree.lua",
  dependencies = {
    "nvim-tree/nvim-web-devicons", -- optional, for file icons
  },
  config = function()
    local nvim_tree = require("nvim-tree")
    local utils = require("sumanthyedoti.utils")
    local map = utils.map_key

    -- recommended settings from nvim-tree documentation
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1

    -- change color for arrows in tree to light blue
    vim.cmd([[ highlight NvimTreeFolderArrowClosed guifg=#3FC5FF ]])
    vim.cmd([[ highlight NvimTreeFolderArrowOpen guifg=#3FC5FF ]])

    -- close NvimTree if last buffer is closed
    vim.api.nvim_create_autocmd("BufEnter", {
      command = "if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif",
      nested = true,
    })

    nvim_tree.setup({
      filters = {
        dotfiles = false,
        custom = { ".DS_Store" },
        exclude = {},
      },
      git = {
        enable = true,
        ignore = true,
        timeout = 500,
      },
      disable_netrw = true,
      hijack_netrw = true,
      open_on_tab = false,
      hijack_cursor = false,
      update_cwd = true,
      hijack_directories = {
        enable = true,
        auto_open = true,
      },
      diagnostics = {
        enable = true,
        icons = {
          hint = "",
          info = "",
          warning = "",
          error = "",
        },
      },
      update_focused_file = {
        enable = true,
        update_cwd = true,
        ignore_list = {},
      },
      view = {
        width = 40,
        relativenumber = true,
        side = "left",
        adaptive_size = true,
        number = false,
      },
      -- disable window_picker for explorer to work well with window splits
      actions = {
        open_file = {
          window_picker = {
            enable = false,
          },
        },
      },
      renderer = {
        indent_markers = {
          enable = true,
        },
        highlight_git = true,
        root_folder_modifier = ":t",
        icons = {
          show = {
            file = true,
            folder = true,
            folder_arrow = true,
            git = true,
          },
          glyphs = {
            default = "",
            symlink = "",
            git = {
              unstaged = "",
              staged = "S",
              unmerged = "",
              renamed = "➜",
              deleted = "",
              untracked = "U",
              ignored = "◌",
            },
            folder = {
              default = "",
              open = "",
              empty = "",
              empty_open = "",
              symlink = "",
              -- arrow_closed = "",
              -- arrow_open = "",
            },
          },
        },
      },
    })

    map("n", "<leader>ee", ":NvimTreeToggle<cr>", { desc = "File Explorer" })
    map("n", "<leader>ef", "<cmd>NvimTreeFindFile<CR><C-o>", { desc = "Toggle file explorer on current file" })
    map("n", "<leader>ec", "<cmd>NvimTreeCollapse<CR>", { desc = "Collapse file explorer" })
    map("n", "<leader>er", "<cmd>NvimTreeRefresh<CR>", { desc = "Refresh file explorer" })
  end,
}
