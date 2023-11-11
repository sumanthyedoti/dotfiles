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
      { ",sb", ":%SlimeSend<cr>", mode = { "n" }, desc = "Slime Buffer" },
      { ",sp", "mzvip:'<,'>SlimeSend<cr>`z", mode = { "n" }, desc = "Slime Root Form (Function)" },
      { ",ss", ":'<,'>SlimeSend<cr>", mode = { "v" }, desc = "Slime Visual" },
      { ",sv", ":'<,'>SlimeSend<cr>", mode = { "v" }, desc = "Slime Visual" },
      { ",sl", ":SlimeSendCurrentLine<cr>", mode = { "n" }, desc = "Slime Current Line" },
    },
    config = function()
      vim.cmd([[let g:slime_target = "tmux"]])
    end,
  },
  {
    "Olical/conjure",
    ft = conjure_filetypes,
    keys = {
      { ",cb", ":ConjureEvalBuf<cr>", mode = { "n" }, desc = "Eval Buffer" },
      { ",cp", ":ConjureEvalCurrentForm<cr>", mode = { "n", "v" }, desc = "Eval Current Form" },
      { ",c ", ":ConjureEvalCurrentForm<cr>", mode = { "n", "v" }, desc = "Eval Current Form" },
      { ",cr", ":ConjureEvalRootForm<cr>", mode = { "n", "v" }, desc = "Eval Root Form" },
      { ",cc", ":ConjureEvalCommentCurrentForm<cr>", mode = { "n", "v" }, desc = "Eval Current Form and Comment" },
      { ",cC", ":ConjureEvalCommentRootForm<cr>", mode = { "n", "v" }, desc = "Eval Root Form and Comment" },
      { ",cc", ":'<,'>ConjureEvalVisual<cr>", mode = { "v" }, desc = "Eval Visual" },
      { ",cv", ":'<,'>ConjureEvalVisual<cr>", mode = { "v" }, desc = "Eval Visual" },
      { ",cl", "mzV:ConjureEvalVisual<cr><esc>`z", mode = { "n" }, desc = "Eval Current Line" },
      { ",cm", ":ConjureEvalMotion<cr>", mode = { "n" }, desc = "Eval Motion" },
      { ",cw", ":ConjureEvalWord<cr>", mode = { "n" }, desc = "Eval Word" },
    },
  },
}
