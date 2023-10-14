return {
  {
    "kdheepak/lazygit.nvim",
    -- optional for floating window border decoration
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    keys = {
      { "<leader>gL", ":LazyGit<cr>", mode = { "n", "v" }, desc = "Lazygit" },
    },
  },
}
