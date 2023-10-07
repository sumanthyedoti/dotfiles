local m_status_ok, lint = pcall(require, "lint")
if not m_status_ok then
	return
end

lint.linters_by_ft = {
  javascript = {"eslint_d"},
  typescript = {"eslint_d"},
}

vim.api.nvim_create_autocmd({ "BufWrite" }, {
  callback = function ()
    require("lint").try_lint()
  end
})

