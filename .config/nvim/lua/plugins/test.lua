return {
  { "nvim-neotest/neotest-plenary" },
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/neotest-jest",
      "marilari88/neotest-vitest",
      "jfpedroza/neotest-elixir",
      "nvim-neotest/neotest-go",
      "nvim-neotest/neotest-python",
    },
    opts = {
      adapters = {
        "neotest-plenary",
        "neotest-jest",
        "neotest-vitest",
        "neotest-elixir",
        "neotest-go",
        "neotest-python",
      },
    },
  },
  { "nvim-neotest/nvim-nio" },
  {
    "mfussenegger/nvim-dap",
    optional = true,
    -- stylua: ignore
    keys = {
      { "<leader>td", function() require("neotest").run.run({strategy = "dap"}) end, desc = "Debug Nearest" },
    },
  },
}
