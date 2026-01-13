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
      {
        "<localleader>sb",
        function()
          vim.cmd("silent %SlimeSend")
        end,
        mode = { "n" },
        desc = "Slime Buffer",
      },
      {
        "<localleader>sp",
        function()
          vim.cmd("normal! mzvip")
          vim.cmd("silent '<,'>SlimeSend")
          vim.cmd("normal! `z")
        end,
        mode = { "n" },
        desc = "Slime Root Form (Function)",
      },
      {
        "<localleader>ss",
        function()
          vim.cmd("silent '<,'>SlimeSend")
        end,
        mode = { "v" },
        desc = "Slime Visual",
      },
      {
        "<localleader>sv",
        function()
          vim.cmd("silent '<,'>SlimeSend")
        end,
        mode = { "v" },
        desc = "Slime Visual",
      },
      {
        "<localleader>ss",
        function()
          vim.cmd("silent SlimeSendCurrentLine")
        end,
        mode = { "n" },
        desc = "Slime Current Line",
      },
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
