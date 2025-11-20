return {
  "folke/flash.nvim",
  event = "VeryLazy",
  opts = {
    modes = {
      char = {
        enabled = true, -- enable Flash motions
        keys = { "f", "F", "t", "T" }, -- only these keys, no "s"/"S"
      },
    },
  },
  keys = {
    { "s", false }, -- disable LazyVim's default "s"
    { "S", false }, -- disable LazyVim's default "S"
    {
      "<localleader>s",
      mode = { "n", "x", "o" },
      function()
        require("flash").jump()
      end,
      desc = "Flash",
    },
    {
      "<localleader>S",
      mode = { "n", "x", "o" },
      function()
        require("flash").treesitter()
      end,
      desc = "Flash Treesitter",
    },
  },
}
