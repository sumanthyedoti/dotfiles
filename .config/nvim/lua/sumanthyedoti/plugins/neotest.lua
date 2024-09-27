return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    "nvim-neotest/neotest-jest",
    "marilari88/neotest-vitest",
    "jfpedroza/neotest-elixir",
    "nvim-neotest/neotest-go",
    "nvim-neotest/neotest-python",
    "nvim-neotest/neotest-vim-test",
  },
  config = function(_, opts)
    local neotest = require("neotest")
    neotest.setup({
      adapters = {
        require("neotest-jest"),
        require("neotest-vitest"),
        require("neotest-elixir"),
        require("neotest-go"),
        require("neotest-python"),
        require("neotest-vim-test")({
          ignore_file_types = { "python", "vim", "lua" },
        }),
      },
    })
  end,
}
