return {
  "NeogitOrg/neogit",
  keys = {
    { "<leader>gn", ":Neogit<CR>", mode = "n", desc = "Neogit" },

    { "<leader>gd", group = "Diffview", desc = "Diffview" }, -- group
    -- Diffview
    { "<leader>gdo", "<cmd>DiffviewOpen<cr>", desc = "Diffview Open", mode = "n" },
    { "<leader>gdc", "<cmd>DiffviewClose<cr>", desc = "Diffview Close", mode = "n" },
    { "<leader>gdh", "<cmd>DiffviewFileHistory<cr>", desc = "Diffview File History", mode = "n" },
    { "<leader>gdt", "<cmd>DiffviewToggleFiles<cr>", desc = "Diffview Toggle Files", mode = "n" },
    { "<leader>gdf", "<cmd>DiffviewFocusFiles<cr>", desc = "Diffview Focus Files", mode = "n" },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "sindrets/diffview.nvim",
    "folke/snacks.nvim",
  },
}

-- `Neogit kind=floating` for floating window
