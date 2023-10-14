---- commands: https://github.com/pwntester/octo.nvim#-commands
return {
  "pwntester/octo.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "nvim-tree/nvim-web-devicons",
  },
  keys = {
    { "<leader>gO", ":Octo<CR>", mode = { "n", "t" }, desc = "Octo - issues and PRs" },
  },
  config = function()
    require("octo").setup({ enable_builtin = true })
    -- vim.cmd([[hi OctoEditable guibg=none]])
  end,
}
