return {
  "elixir-tools/elixir-tools.nvim",
  priority = 0,
  ft = { "elixir" },
  version = "*",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local elixir = require("elixir")

    local elixirls = require("elixir.elixirls")

    elixir.setup({
      nextls = { enable = true },
      credo = {},
      elixirls = {
        enable = true,
        settings = elixirls.settings({
          dialyzerEnabled = false,
          enableTestLenses = false,
        }),
        on_attach = function(client, bufnr)
          vim.keymap.set("n", "<localleader>eP", ":ElixirFromPipe<cr>", { buffer = true, noremap = true })
          vim.keymap.set("n", "<localleader>ep", ":ElixirToPipe<cr>", { buffer = true, noremap = true })
          vim.keymap.set("v", "<localleader>ee", ":ElixirExpandMacro<cr>", { buffer = true, noremap = true })
        end,
      },
    })

  end,
}
