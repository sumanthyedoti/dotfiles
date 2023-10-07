return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 500
  end,
  opts = {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  },
  config = function()
    local wk = require("which-key")

    vim.cmd([[nnoremap <leader>xd "_dd]])
    vim.cmd([[nnoremap <leader>xD "_D]])
    vim.cmd([[nnoremap <leader>xc "_cc]])
    vim.cmd([[nnoremap <leader>xC "_C]])
    vim.cmd([[nnoremap <leader>xx "_x]])
    vim.cmd([[vnoremap <leader>xd "_d]])
    vim.cmd([[vnoremap <leader>xc "_c]])
    wk.register({
      x = {
        name = "Delete without yank",
        d = { '"_dd', "Delete line" },
        D = { '"_D', "Delete line from cursor" },
        c = { '"_cc', "Change line" },
        x = { '"_xx', "Delete char" },
      },
    }, { prefix = "<leader>" })

    wk.register({
      f = {
        name = "Find",
        -- keys are defined in `plugins/init.lua`
      },
    }, { prefix = "," })

    wk.register({
      d = {
        name = "DAP",
        -- keys are defined in `plugins/init.lua`
      },
    }, { prefix = "," })

    wk.register({
      s = {
        name = "Slime",
      },
    }, { prefix = "," })

    wk.register({
      c = {
        name = "Conjure",
      },
    }, { prefix = "," })

    wk.register({
      e = {
        name = "Elixir",
      },
    }, { prefix = "," })

    wk.register({
      e = {
        name = "Conjure",
      },
    }, { prefix = "," })

    wk.register({
      A = {
        name = "AI",
      },
    }, { prefix = "<leader>" })

    wk.register({
      D = {
        name = "Database",
        u = { "<Cmd>DBUIToggle<Cr>", "Toggle UI" },
        f = { "<Cmd>DBUIFindBuffer<Cr>", "Find buffer" },
        r = { "<Cmd>DBUIRenameBuffer<Cr>", "Rename buffer" },
        q = { "<Cmd>DBUILastQueryInfo<Cr>", "Last query info" },
      },
    })

    wk.register({
      y = {
        name = "Terminal",
      },
    }, { prefix = "<leader>" })

    wk.register({
      v = {
        name = "Vim",
        r = { ":reg<Cr>", "Registers" },
      },
    }, { prefix = "<leader>" })

    wk.register({
      t = {
        name = "Terminal",
      },
    }, { prefix = "<leader>" })

    wk.register({
      T = {
        name = "Toggle",
        w = { "<cmd>set wrap!<CR>", "Line Wrap ↔" },
        r = { "<cmd>set number<cr><cmd>set relativenumber!<CR>", "Relative Number ↔" },
      },
    }, { prefix = "<leader>" })

    wk.register({
      w = {
        name = "Windfow",
      },
    }, { prefix = "<leader>" })

  end,
}
