return {
  "vim-test/vim-test",
  config = function()
    vim.cmd([[
      nmap <silent> ctt :TestNearest<CR>
      nmap <silent> ctf :TestFile<CR>
      nmap <silent> cta :TestSuite<CR>
      nmap <silent> ctl :TestLast<CR>
      nmap <silent> ctg :TestVisit<CR>
    ]])
  end,
}
