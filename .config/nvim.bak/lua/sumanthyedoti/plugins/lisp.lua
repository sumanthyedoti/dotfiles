local lisp_filetypes = { "lisp", "lsp", "el" }

return {
  { "clojure-vim/vim-jack-in", dependencies = {
    "tpope/vim-dispatch",
  } },
  -- ## lisp
  { "gpanders/nvim-parinfer", ft = lisp_filetypes },
  { "guns/vim-sexp", ft = lisp_filetypes },
  { "tpope/vim-sexp-mappings-for-regular-people", ft = lisp_filetypes },
}
