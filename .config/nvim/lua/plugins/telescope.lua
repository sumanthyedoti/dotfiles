return {
  {
    "nvim-telescope/telescope.nvim",
    opts = function(_, opts)
      local map = vim.keymap.set
      map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Buffers" })
      map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { desc = "Buffers" })
      map("n", ",fb", function()
        picker.buffers()
      end, { desc = "Buffers (Snacks)" })
      map("n", ",fr", function()
        picker.recent()
      end, { desc = "Recent (Snacks)" })
    end,
  },
}
