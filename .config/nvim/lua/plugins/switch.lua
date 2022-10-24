local opts = { noremap = true, silent = true }
vim.api.nvim_set_keymap("n", "<leader>`", ":Switch<CR>", opts)

vim.g.switch_custom_definitions = {
  {"_", "-"} -- TODO: fix
}
