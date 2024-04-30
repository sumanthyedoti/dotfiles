return {
  "NeogitOrg/neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "sindrets/diffview.nvim",
  },
  keys = {
    { "<leader>gg", ":Neogit<cr>", mode = { "n", "v" }, desc = "Neogit" },
    { "<leader>gR", ":NeogitResetState<cr>", mode = { "n", "v" }, desc = "Neogit reset" },
    { "<leader>gc", ":Neogit commit<cr>", mode = { "n", "v" }, desc = "Neogit commit" },
    { "<leader>gp", ":Neogit pull<cr>", mode = { "n", "v" }, desc = "Neogit pull" },
    { "<leader>gu", ":Neogit push<cr>", mode = { "n", "v" }, desc = "Neogit push" },
    { "<leader>gl", ":Neogit log<cr>", mode = { "n", "v" }, desc = "Neogit log" },
  },
  config = true,
}
