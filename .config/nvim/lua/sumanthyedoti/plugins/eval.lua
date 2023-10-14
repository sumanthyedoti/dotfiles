-- 🌐  https://github.com/Olical/conjure/wiki/
local conjure_filetypes = {
  "lisp",
  "lsp",
  "clojure",
  "clojurescript",
  "haskell",
  "scheme",
  "racket",
  "rust",
  "python",
  "hy",
  "lua",
  "fennel",
}

local repl_filetypes = {
  "lisp",
  "lsp",
  "scheme",
  "el",
  "clojure",
  "haskell",
  "elm",
  "ocaml",
  "javascript",
  "javascriptreact",
  "typescript",
  "typescriptreact",
  "python",
  "elixir",
}

return {
  {
    "jpalardy/vim-slime",
    ft = repl_filetypes,
    keys = {
      { ",sb", ":%SlimeSend<cr>", mode = { "n" } },
      { ",sp", "mzvip:'<,'>SlimeSend<cr>`z", mode = { "n" } },
      { ",sv", ":'<,'>SlimeSend<cr>", mode = { "v" } },
      { ",sl", ":SlimeSendCurrentLine<cr>", mode = { "n" } },
    },
    config = function()
      vim.cmd([[let g:slime_target = "tmux"]])
    end,
  },
  {
    "Olical/conjure",
    ft = conjure_filetypes,
    keys = {
      { ",cb", ":ConjureEvalBuf<cr>", mode = { "n" } },
      { ",cp", ":ConjureEvalCurrentForm<cr>", mode = { "n", "v" } },
      { ",cr", ":ConjureEvalRootForm<cr>", mode = { "n", "v" } },
      { ",cc", ":ConjureEvalCommentCurrentForm<cr>", mode = { "n", "v" } },
      { ",cC", ":ConjureEvalCommentRootForm<cr>", mode = { "n", "v" } },
      { ",cv", ":'<,'>ConjureEvalVisual<cr>", mode = { "v" } },
      { ",cl", "mzV:ConjureEvalVisual<cr><esc>`z", mode = { "n" } },
      { ",cm", ":ConjureEvalMotion<cr>", mode = { "n" } },
      { ",cw", ":ConjureEvalWord<cr>", mode = { "n" } },
    },
  },
}
