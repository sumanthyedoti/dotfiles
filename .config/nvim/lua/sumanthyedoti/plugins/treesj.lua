return {
  "Wansmer/treesj",
  keys = {
    { "<leader>tj", ":TSJToggle<cr>", desc = "TSJToggle" },
  },
  dependencies = { "nvim-treesitter/nvim-treesitter" },
  config = function()
    require("treesj").setup({--[[ your config ]]
    })
  end,
}

-- [[
-- f for add/change/delete function name
-- ]]
