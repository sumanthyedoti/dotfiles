return {
  "akinsho/toggleterm.nvim",
  keys = {
    { "<leader>tt", ":ToggleTerm<CR>", mode = { "n", "t" } },
    -- { "<leader>tt", ":Lspsaga term_toggle<CR>", mode = { "n", "t" } },
    { "<leader>tg", ":lua _LAZYGIT_TOGGLE()<CR>", mode = { "n", "t" } },
    { "<leader>th", ":lua _HTOP_TOGGLE()<CR>", mode = { "n", "t" } },
    { "<leader>td", ":lua _NCDU_TOGGLE()<CR>", mode = { "n", "t" } },
    { "<leader>tn", ":lua _NODE_TOGGLE()<CR>", mode = { "n", "t" } },
    { "<leader>tx", ":lua _ELIXIR_TOGGLE()<CR>", mode = { "n", "t" } },
    { "<leader>tp", ":lua _PYTHON_TOGGLE()<CR>", mode = { "n", "t" } },
    { "<leader>tc", ":lua _CHT_SH()<CR>", mode = { "n", "t" } },
  },
  config = function()
    local toggleterm = require("toggleterm")

    toggleterm.setup({
      size = 20,
      open_mapping = [[<leader>tt]],
      hide_numbers = true,
      shade_filetypes = {},
      shade_terminals = true,
      shading_factor = 2,
      start_in_insert = true,
      insert_mappings = true,
      persist_size = false,
      direction = "float",
      close_on_exit = true,
      shell = vim.o.shell,
      float_opts = {
        border = "curved",
        winblend = 0,
        highlights = {
          border = "Normal",
          background = "Normal",
        },
      },
    })

    function _G.set_terminal_keymaps()
      local opts = { noremap = true }
      vim.api.nvim_buf_set_keymap(0, "t", "<esc>", [[<C-\><C-n>]], opts)
      vim.api.nvim_buf_set_keymap(0, "t", "<C-h>", [[<C-\><C-n><C-W>h]], opts)
      vim.api.nvim_buf_set_keymap(0, "t", "<C-j>", [[<C-\><C-n><C-W>j]], opts)
      vim.api.nvim_buf_set_keymap(0, "t", "<C-k>", [[<C-\><C-n><C-W>k]], opts)
      vim.api.nvim_buf_set_keymap(0, "t", "<C-l>", [[<C-\><C-n><C-W>l]], opts)
    end

    vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")

    local Terminal = require("toggleterm.terminal").Terminal

    local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })
    function _LAZYGIT_TOGGLE()
      lazygit:toggle()
    end

    local node = Terminal:new({ cmd = "node", hidden = true })
    function _NODE_TOGGLE()
      node:toggle()
    end

    local elixir = Terminal:new({ cmd = "iex", hidden = true })
    function _ELIXIR_TOGGLE()
      elixir:toggle()
    end

    local ncdu = Terminal:new({ cmd = "ncdu", hidden = true })
    function _NCDU_TOGGLE()
      ncdu:toggle()
    end

    local htop = Terminal:new({ cmd = "htop", hidden = true })
    function _HTOP_TOGGLE()
      htop:toggle()
    end

    local python = Terminal:new({ cmd = "python", hidden = true })
    function _PYTHON_TOGGLE()
      python:toggle()
    end

    local cht = Terminal:new({ cmd = "cht.sh --shell", hidden = true })
    function _CHT_SH()
      cht:toggle()
    end
  end,
}
