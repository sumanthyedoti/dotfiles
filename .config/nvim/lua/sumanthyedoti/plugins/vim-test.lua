return {
  "vim-test/vim-test",
  dependencies = {
    "preservim/vimux",
  },
  config = function()
    vim.keymap.set("n", "ctt", ":TestNearest<CR>")
    vim.keymap.set("n", "ctf", ":TestFile<CR>")
    vim.keymap.set("n", "cta", ":TestSuite<CR>")
    vim.keymap.set("n", "ctl", ":TestLast<CR>")
    vim.keymap.set("n", "ctg", ":TestVisit<CR>")

    vim.cmd("let test#strategy = 'vimux'")
    vim.cmd("let test#preserve_screen = 0")
  end,
}
