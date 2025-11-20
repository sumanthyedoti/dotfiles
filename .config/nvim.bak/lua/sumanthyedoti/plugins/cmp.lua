return {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    "saadparwaiz1/cmp_luasnip", -- snippet autocompletions
    "hrsh7th/cmp-buffer", -- buffer completions
    "hrsh7th/cmp-path", -- path completions
    "hrsh7th/cmp-cmdline", -- cmdline completions
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-nvim-lua", -- for neovim Lua API
    -- "hrsh7th/cmp-emoji",
    "onsails/lspkind.nvim",
    "petertriho/cmp-git",
  },
  config = function()
    local cmp = require("cmp")
    local luasnip = require("luasnip")
    local lspkind = require("lspkind")

    require("cmp_git").setup()
    local cmp_autopairs = require("nvim-autopairs.completion.cmp")

    lspkind.init({
      mode = "symbol", -- 'text', 'text_symbol', 'symbol_text', 'symbol'
      preset = "codicons",
      symbol_map = {
        Text = "󰉿",
        Method = "󰆧",
        Function = "󰊕",
        Constructor = "",
        Field = "󰜢",
        Variable = "󰀫",
        Class = "󰠱",
        Interface = "",
        Module = "",
        Property = "󰜢",
        Unit = "󰑭",
        Value = "󰎠",
        Enum = "",
        Keyword = "󰌋",
        Snippet = "",
        Color = "󰏘",
        File = "󰈙",
        Reference = "󰈇",
        Folder = "󰉋",
        EnumMember = "",
        Constant = "󰏿",
        Struct = "󰙅",
        Event = "",
        Operator = "󰆕",
        TypeParameter = "",
        Codeium = "",
        Dadbod = "",
      },
    })

    require("luasnip/loaders/from_vscode").lazy_load()

    vim.opt.completeopt = "menu,menuone,noselect"

    local check_backspace = function()
      local col = vim.fn.col(".") - 1
      return col == 0 or vim.fn.getline("."):sub(col, col):match("%s")
    end

    cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

    -- 🌐 https://github.com/neovim/nvim-lspconfig/wiki/Snippets
    cmp.setup({
      snippet = {
        -- REQUIRED - must specify a snippet engine
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      window = {
        completion = cmp.config.window.bordered({
          border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
          -- border = "double",
        }),
        documentation = {
          border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
        },
      },
      mapping = {
        ["<C-k>"] = cmp.mapping.select_prev_item(),
        ["<C-j>"] = cmp.mapping.select_next_item(),
        ["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
        ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
        ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
        ["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
        ["<C-o>"] = cmp.mapping.close(),
        -- Accept currently selected item. If none selected, `select` first item.
        -- Set `select` to `false` to only confirm explicitly selected items.
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif luasnip.expandable() then
            luasnip.expand()
          elseif luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          elseif check_backspace() then
            fallback()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { "i", "s" }),
      },
      -- HERE:
      sources = cmp.config.sources({
        {
          name = "nvim_lsp",
          max_item_count = 6,
          entry_filter = function(entry)
            return require("cmp").lsp.CompletionItemKind.Snippet ~= entry:get_kind()
          end,
        },
        { name = "codeium" },
        { name = "nvim_lua", max_item_count = 4 },
        { name = "vim-dadbod-completion" },
        { name = "path", keyword_length = 1, max_item_count = 6 },
        { name = "luasnip", max_item_count = 6 },
        { name = "orgmode" },
        -- { name = "emoji", max_item_count = 6 },
        { name = "git" },
      }, {
        { name = "buffer", max_item_count = 4, keyword_length = 3 },
      }),
      formatting = {
        fields = { "kind", "abbr", "menu" },
        format = lspkind.cmp_format({
          with_text = true,
          menu = {
            nvim_lsp = "[lsp]",
            codeium = "[co]",
            nvim_lua = "[nLua]",
            luasnip = "[snip]",
            buffer = "[buff]",
            -- emoji = "emoji",
            path = "[path]",
            cmp_git = "[git]",
          },
          ellipsis_char = "...",
          -- The function below will be called before any actual modifications from lspkind
          before = function(_entry, vim_item)
            -- ...
            return vim_item
          end,
        }),
      },
      confirm_opts = {
        behavior = cmp.ConfirmBehavior.Replace,
        select = false,
      },
      experimental = {
        new_menu = true,
        ghost_text = false,
      },
    })
  end,
}
