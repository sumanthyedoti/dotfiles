return {
  {
    "folke/snacks.nvim",
    opts = function(_, opts)
      local map = vim.keymap.set
      local picker = require("snacks").picker
      map("n", ",ff", function()
        picker.files()
      end, { desc = "Find files (Snacks)" })
      map("n", ",/", function()
        picker.grep()
      end, { desc = "Grep (Snacks)" })
      map("n", ",fb", function()
        picker.buffers()
      end, { desc = "Buffers (Snacks)" })
      map("n", ",fr", function()
        picker.recent()
      end, { desc = "Recent (Snacks)" })
      map("n", ",fr", function()
        picker.recent()
      end, { desc = "Recent (Snacks)" })
      map("n", ",sk", function()
        picker.keymaps()
      end, { desc = "Recent (Snacks)" })
    end,
  },
}
