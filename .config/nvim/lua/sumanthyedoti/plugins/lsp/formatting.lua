local conform = require("conform")

conform.setup({
  formatters_by_ft = {
    lua = { "stylua" },
    -- Conform will run multiple formatters sequentially
    python = { "isort", "black" },
    -- Use a sub-list to run only the first available formatter
    javascript = { { "prettierd", "prettier" } },
    typescript = { { "prettierd", "prettier" } },
    javascriptreact = { { "prettierd", "prettier" } },
    typescriptreact = { { "prettierd", "prettier" } },
    svelte = { { "prettierd", "prettier" } },
    css = { { "prettierd", "prettier" } },
    html = { { "prettierd", "prettier" } },
    json = { { "prettierd", "prettier" } },
    yaml = { { "prettierd", "prettier" } },
    markdown = { { "prettierd", "prettier" } },
    graphql = { { "prettierd", "prettier" } },
  },
  format_on_save = {
    lsp_fallback = true,
    async = false,
    timeout_ms = 700,
  },

  vim.keymap.set({ "n", "v" }, "<leader>lF", function()
    conform.format({
      lsp_fallback = true,
      async = false,
      timeout_ms = 700,
    })
  end, { desc = "Format code" }),
})
