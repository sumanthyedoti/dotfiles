return {
  "nvim-treesitter/nvim-treesitter",
  lazy = true,
  build = ":TSUpdate",
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    "HiPhish/nvim-ts-rainbow2",
    "nvim-treesitter/playground",
    "windwp/nvim-ts-autotag",
    "nvim-treesitter/nvim-treesitter-context",
    "nvim-treesitter/nvim-treesitter-textobjects",
  },
  config = function()
    local treesitter = require("nvim-treesitter.configs")

    treesitter.setup({
      ensure_installed = {
        "bash",
        "c",
        "clojure",
        "cmake",
        "commonlisp",
        "cpp",
        "css",
        "dart",
        "dockerfile",
        "eex",
        "elixir",
        "elm",
        "erlang",
        "fish",
        "go",
        "graphql",
        "haskell",
        "hcl",
        "heex",
        "html",
        "http",
        "java",
        "javascript",
        "jsdoc",
        "json",
        -- "kotlin",
        "latex",
        "llvm",
        "lua",
        "make",
        "markdown",
        "markdown_inline",
        -- "ocaml",
        -- "ocaml_interface",
        -- "ocamllex",
        "org",
        "prisma",
        "python",
        "regex",
        "rust",
        "scheme",
        "scss",
        "sql",
        "svelte",
        "toml",
        "tsx",
        "typescript",
        "vim",
        "vue",
        "yaml",
      },
      matchup = {
        enable = true, -- mandatory, false will disable the whole extension
        disable = {}, -- optional, list of language that will be disabled
        -- [options]
      },
      auto_install = true,
      sync_install = false,
      ignore_install = { "" }, -- List of parsers to ignore installing
      highlight = {
        enable = true, -- false will disable the whole extension -- HERE:
        disable = { "" }, -- list of language that will be disabled
        additional_vim_regex_highlighting = { "org" },
      },
      indent = {
        enable = true,
        -- disable = { "yaml" },
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<C-space>",
          node_incremental = "<C-space>",
          scope_incremental = false,
          node_decremental = "<BS>",
        },
      },
      autotag = {
        enable = true,
      },
      context_commentstring = {
        enable = true,
      },
      -- rainbow = { -- HERE:
      -- 	enable = true,
      -- 	-- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
      -- 	extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
      -- 	max_file_lines = nil, -- Do not enable for files with more than n lines, int
      -- 	-- Which query to use for finding delimiters
      -- 	query = "rainbow-parens",
      -- 	-- colors = {}, -- table of hex strings
      -- 	-- termcolors = {} -- table of colour name strings
      -- 	strategy = require("ts-rainbow").strategy.global,
      -- },
      playground = {
        enable = true,
        disable = {},
        updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
        persist_queries = false, -- Whether the query persists across vim sessions
        keybindings = {
          toggle_query_editor = "o",
          toggle_hl_groups = "i",
          toggle_injected_languages = "t",
          toggle_anonymous_nodes = "a",
          toggle_language_display = "I",
          focus_language = "f",
          unfocus_language = "F",
          update = "R",
          goto_node = "<cr>",
          show_help = "?",
        },
      },
      textobjects = { -- 🌐 https://github.com/nvim-treesitter/nvim-treesitter-textobjects
        select = {
          enable = true,

          -- Automatically jump forward to textobj, similar to targets.vim
          lookahead = true,

          keymaps = {
            -- You can use the capture groups defined in textobjects.scm
            ["a="] = { query = "@assignment.outer", desc = "Select Assignment outer" },
            ["i="] = { query = "@assignment.inner", desc = "Select Assignment inner" },
            ["l="] = { query = "@assignment.lhs", desc = "Select Assignment outer" },
            ["r="] = { query = "@assignment.rhs", desc = "Select Assignment innrer" },
            ["ai"] = { query = "@call.outer", desc = "Select Function outer" },
            ["ii"] = { query = "@call.inner", desc = "Select Function inner" },
            ["af"] = { query = "@function.outer", desc = "Select Function/Method outer" },
            ["if"] = { query = "@function.inner", desc = "Select Function/Method inner" },
            ["aa"] = { query = "@parameter.outer", desc = "Select Parameter outer" },
            ["ia"] = { query = "@parameter.inner", desc = "Select Parameter innrer" },
            ["ab"] = { query = "@conditional.outer", desc = "Select Conditional outer" },
            ["ib"] = { query = "@conditional.inner", desc = "Select Conditional inner" },
            ["al"] = { query = "@loop.outer", desc = "Select Loop outer" },
            ["il"] = { query = "@loop.inner", desc = "Select Loop inner" },
            ["ac"] = { query = "@comment.outer", desc = "Select Comment" },

            ["am"] = "@class.outer",
            -- You can optionally set descriptions to the mappings (used in the desc parameter of
            -- nvim_buf_set_keymap) which plugins like which-key display
            ["im"] = { query = "@class.inner", desc = "Select inner part of a class region" },
            -- You can also use captures from other query groups like `locals.scm`
            ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
          },
          -- You can choose the select mode (default is charwise 'v')
          --
          -- Can also be a function which gets passed a table with the keys
          -- * query_string: eg '@function.inner'
          -- * method: eg 'v' or 'o'
          -- and should return the mode ('v', 'V', or '<c-v>') or a table
          -- mapping query_strings to modes.
          selection_modes = {
            ["@parameter.outer"] = "v", -- charwise
            ["@function.outer"] = "V", -- linewise
            ["@class.outer"] = "<c-v>", -- blockwise
          },
          -- If you set this to `true` (default is `false`) then any textobject is
          -- extended to include preceding or succeeding whitespace. Succeeding
          -- whitespace has priority in order to act similarly to eg the built-in
          -- `ap`.
          --
          -- Can also be a function which gets passed a table with the keys
          -- * query_string: eg '@function.inner'
          -- * selection_mode: eg 'v'
          -- and should return true of false
          include_surrounding_whitespace = true,
        },
        swap = {
          enable = true,
          swap_next = {
            ["<localleader>j"] = "@parameter.inner",
            ["<localleader>n"] = "@function.outer",
          },
          swap_previous = {
            ["<localleader>k"] = "@parameter.inner",
            ["<localleader>m"] = "@function.outer",
          },
        },
        move = {
          enable = true,
          set_jumps = true, -- whether to set jumps in the jumplist
          goto_next_start = {
            ["]i"] = { query = "@call.outer", desc = "Goto next Call" },
            ["]f"] = { query = "@function.outer", desc = "Goto next Function start" },
            ["]m"] = { query = "@class.outer", desc = "Next Class start" },
            ["]s"] = { query = "@scope", query_group = "locals", desc = "Next scope start" },
            ["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
          },
          goto_previous_start = {
            ["[i"] = { query = "@call.outer", desc = "prev Call" },
            ["[f"] = { query = "@function.outer", desc = "Prev Function start" },
            ["[m"] = { query = "@class.outer", desc = "Prev Class start" },
            ["[s"] = { query = "@scope", query_group = "locals", desc = "Prev scope start" },
            ["[z"] = { query = "@fold", query_group = "folds", desc = "Prev fold" },
          },
          goto_next_end = {
            ["]F"] = "@function.outer",
            ["]M"] = "@class.outer",
            ["]S"] = { query = "@scope", query_group = "locals", desc = "Next scope end" },
          },
          goto_previous_end = {
            ["[F"] = "@function.outer",
            ["[M"] = "@class.outer",
            ["[S"] = { query = "@scope", query_group = "locals", desc = "Prev scope end" },
          },
          goto_next = {
            ["]b"] = "@conditional.outer",
            ["]o"] = "@loop.*",
          },
          goto_previous = {
            ["[b"] = "@conditional.outer",
            ["[o"] = "@loop.*",
          },
        },
        lsp_interop = {
          enable = true,
          border = "none",
          floating_preview_opts = {},
          peek_definition_code = {
            ["<leader>od"] = "@function.outer",
            ["<leader>oD"] = "@class.outer",
          },
        },
      },
    })

    --[[ make the movements repeatable with ; and , ]]
    local ts_repeat_move = require("nvim-treesitter.textobjects.repeatable_move")

    -- Repeat movement with ; and ,
    -- ensure ; goes forward and , goes backward regardless of the last direction
    vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move_next)
    vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_previous)

    -- vim way: ; goes to the direction you were moving.
    vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move)
    vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_opposite)

    -- Optionally, make builtin f, F, t, T also repeatable with ; and ,
    vim.keymap.set({ "n", "x", "o" }, "f", ts_repeat_move.builtin_f)
    vim.keymap.set({ "n", "x", "o" }, "F", ts_repeat_move.builtin_F)
    vim.keymap.set({ "n", "x", "o" }, "t", ts_repeat_move.builtin_t)
    vim.keymap.set({ "n", "x", "o" }, "T", ts_repeat_move.builtin_T)

    vim.keymap.set("n", "<leader>tp", "<cmd>TSPlaygroundToggle<CR>")
  end,
}
