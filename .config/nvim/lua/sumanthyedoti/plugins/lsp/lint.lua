local m_status_ok, lint = pcall(require, "lint")
if not m_status_ok then
  return
end

lint.linters_by_ft = {
  --[[ JS ]]
  javascript = { "eslint_d" },
  typescript = { "eslint_d" },
  javascriptreact = { "eslint_d" },
  typescriptreact = { "eslint_d" },
  svelte = { "eslint_d" },
  --[[ Python ]]
  python = { "pylint" },
}

local lint_augroup = vim.api.nvim_create_augroup("lint", { clear = true })

vim.api.nvim_create_autocmd({ -- :h events
  "BufEnter",
  "BufWritePost",
  "InsertLeave",
  -- "TextChanged"
}, {
  group = lint_augroup,
  callback = function()
    lint.try_lint()
  end,
})

vim.keymap.set({ "n", "v" }, "<leader>lL", function()
  lint.try_lint()
end, { desc = "Format code" })
