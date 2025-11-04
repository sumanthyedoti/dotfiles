-- 🌐  https://github.com/Olical/conjure/wiki/
local conjure_filetypes = {
  "lisp",
  "lsp",
  "clojure",
  "clojurescript",
  "javascript",
  "typescript",
  "php",
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
      { ",ss", ":SlimeSendCurrentLine<cr>", mode = { "n" }, desc = "Slime Current Line" },
    },
    config = function()
      vim.cmd([[let g:slime_target = "tmux"]])
    end,
  },

  -- [
  -- :ConjureConnect localhost:<PORT>
  -- ]
  {
    "Olical/conjure",
    ft = conjure_filetypes,
  },
}
