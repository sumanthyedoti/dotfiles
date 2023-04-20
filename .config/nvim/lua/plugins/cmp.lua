local cmp_status_ok, cmp = pcall(require, "cmp")
if not cmp_status_ok then
	return
end

local snip_status_ok, luasnip = pcall(require, "luasnip")
if not snip_status_ok then
	return
end

local kind_status_ok, lspkind = pcall(require, "lspkind")
if not kind_status_ok then
	return
end

require("cmp_git").setup()

lspkind.init({
	mode = "symbol", -- 'text', 'text_symbol', 'symbol_text', 'symbol'
	preset = "codicons",
	symbol_map = {
		Text = "",
		Method = "",
		Function = "",
		Constructor = "",
		Field = "ﰠ",
		Variable = "",
		Class = "ﴯ",
		Interface = "",
		Module = "",
		Property = "ﰠ",
		Unit = "塞",
		Value = "",
		Enum = "",
		Keyword = "",
		Snippet = "",
		Color = "",
		File = "",
		Reference = "",
		Folder = "",
		EnumMember = "",
		Constant = "",
		Struct = "פּ",
		Event = "",
		Operator = "",
		TypeParameter = "𝕋",
	},
})

require("luasnip/loaders/from_vscode").lazy_load()

vim.opt.completeopt = "menu,menuone,noselect"

local check_backspace = function()
	local col = vim.fn.col(".") - 1
	return col == 0 or vim.fn.getline("."):sub(col, col):match("%s")
end

-- 🌐 https://github.com/neovim/nvim-lspconfig/wiki/Snippets
cmp.setup({
	snippet = {
		-- REQUIRED - must specify a snippet engine
		expand = function(args)
			luasnip.lsp_expand(args.body) -- For `luasnip`
			vim.fn["vsnip#anonymous"](args.body) -- vsnip
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
		["<C-o>"] = cmp.mapping({
			i = cmp.mapping.abort(),
			c = cmp.mapping.close(),
		}),
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
	sources = {
		{
			name = "nvim_lsp",
			max_item_count = 6,
		},
		{ name = "nvim_lua" },
		{ name = "luasnip" },
		{ name = "vsnip" },
		{ name = "buffer", max_item_count = 6 },
		{ name = "path", keyword_length = 1 },
		{ name = "git" },
	},
	formatting = {
		fields = { "kind", "abbr", "menu" },
		format = lspkind.cmp_format({
			with_text = true,
			menu = {
				nvim_lsp = "[lsp]",
				nvim_lua = "[nLua]",
				luasnip = "[snip]",
				vsnip = "[vsnip]",
				buffer = "[buff]",
				path = "[path]",
				git = "[git]",
			},
			maxwidth = 50,
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
		ghost_text = false,
	},
})
