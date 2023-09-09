-- Automatically install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
	PACKMAN_BOOTSTRAP = vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
	print("Installing Lazy.nvim, close and reopen Neovim...")
end

vim.opt.rtp:prepend(lazypath)

if PACKMAN_BOOTSTRAP then
	require("lazy").sync()
end

-- Use a protected call so we don't error out on first use
local status_ok, lazy = pcall(require, "lazy")
if not status_ok then
	return
end

--########################################
-- Install your plugins here
--  packages are stored in '~/.local/share/nvim/lazy'
--[[ PACKAGES
  dispatch
  neogit
  neo-minimap  - https://youtu.be/vNyQBWfSh7c
  colortils
    color-picker.nvim
  syntax-tree-surfer
  andweeb/presence.nvim (Discord)
  Wansmer/treesj
  dail.nvim
  doom.nvim
  anuvyklack/windows.nvim
  pretty-fold.nvim
  fold-preview.nvim
  cphealper
    CompetiTest.nvim
  alpha-nvim
    neovim-session-manager
  telescope-media-files
  telescope-file-browser
  vim-tmux-navigator
  vim-maximizer
  vim-illuminate
  vim-clap
  prettier.nvim
  vim-dadbod (for sql) [https://youtu.be/_DnmphIwnjo?t=980]
  hologram.nvim
  sleuth.vim
  nvim-notify
  neogit
  overseer.nvim
  GitHub Copilot
  neoformat
  nvim-peekup
  nabla.nvim
  Markdown:
  - vim-markdown
  - vim-markdown-toc
  - Preview:
    - glow
    - markdown-preview
  Writing:
  - goyo.vim
  - true-zen.nvim
  - zen-mode.nvim
  Note taking:
  - vimwiki
  - todo.txt-vim
  - vim-dotoo
  - Taskwarrior + VimWiki + TaskWiki
  - telekasten.nvim
  - Obsidian.nvim
  michaelb/sniprun *
  codi.vim
  TODO:
  - configure gitsigns
  - configure hydra
]]

local lisp_filetypes = { "lisp", "lsp", "el" }
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
local conjure_filetypes = {
	"lisp",
	"lsp",
	"clojure",
	"clojurescript",
	"haskell",
	"scheme",
	"rust",
	"python",
	"lua",
}

local plugins = {
	"nvim-lua/plenary.nvim", -- lua utility functions used by lots of plugins
	"nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
	{ "stevearc/dressing.nvim", event = "VeryLazy" },
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		lazy = true,
		config = function()
			require("plugins.autopairs")
		end,
	},
	{ "mattn/webapi-vim", event = "BufEnter" },
	{ "tpope/vim-surround", lazy = true, event = "BufEnter" },
	{ "andymass/vim-matchup", event = "BufEnter" },
	{
		"ggandor/leap.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.leap")
		end,
		dependencies = {
			"tpope/vim-repeat",
		},
	},
	{
		"AndrewRadev/switch.vim",
		event = "BufEnter",
		config = function()
			require("plugins.switch")
		end,
	},
	{ "nvim-tree/nvim-web-devicons", event = "VeryLazy" },
	{
		"nvim-lualine/lualine.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.lualine")
		end,
	}, --snippet engine
	{
		"dstein64/vim-startuptime",
		cmd = "StartupTime",
		init = function() -- init is called during startup. Configuration for vim plugins typically should be set in an init function
			vim.g.startuptime_tries = 6
		end,
	},
	-- colorschemes
	"EdenEast/nightfox.nvim",
	"ellisonleao/gruvbox.nvim",
	"folke/tokyonight.nvim",
	"bluz71/vim-nightfly-guicolors",
	"navarasu/onedark.nvim",
	"haishanh/night-owl.vim",
	{ "catppuccin/nvim", name = "catppuccin" },

	{
		"ziontee113/icon-picker.nvim",
		event = "BufEnter",
		config = function()
			require("icon-picker").setup({
				disable_legacy_commands = true,
			})
		end,
	},
	{
		"folke/todo-comments.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.todo-comments")
		end,
	},
	{
		"akinsho/bufferline.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.bufferline")
		end,
		version = "v3.*",
		dependencies = "nvim-tree/nvim-web-devicons",
	},
	{
		"akinsho/toggleterm.nvim",
		keys = {
			{ "<leader>tt", ":ToggleTerm<CR>", mode = { "n", "t" } },
			-- { "<leader>tt", ":Lspsaga term_toggle<CR>", mode = { "n", "t" } },
			{ "<leader>tg", ":lua _LAZYGIT_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>th", ":lua _HTOP_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>td", ":lua _NCDU_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>tn", ":lua _NODE_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>tp", ":lua _PYTHON_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>tc", ":lua _CHT_SH()<CR>", mode = { "n", "t" } },
		},
		config = function()
			require("plugins.toggleterm")
		end,
	},
	{
		"moll/vim-bbye", -- to close buffers key -> ⎵bw
		event = "BufEnter",
	},
	-- Commentery
	{
		"numToStr/Comment.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.comment")
		end,
	},

	-- ## NVim Tree file explorer
	{
		"nvim-tree/nvim-tree.lua",
		event = "BufEnter",
		dependencies = {
			"nvim-tree/nvim-web-devicons", -- optional, for file icons
		},
		tag = "nightly", -- optional, updated every week. (see issue #1193)
		config = function()
			require("plugins.nvim-tree")
		end,
	},

	{
		"mattn/emmet-vim",
		ft = { "html", "css", "javascriptreact", "typescriptreact" },
	},
	{ "mg979/vim-visual-multi" },

	{
		"folke/neodev.nvim",
		config = function()
			require("plugins.neodev")
		end,
	},

	--[[ Debugging ]]
	-- use("mfussenegger/nvim-dap")
	-- use({ "rcarriga/nvim-dap-ui", dependencies = { "" }, config = "require 'plugins.dapui'" })

	--[[ LSP ]]
	{ "neovim/nvim-lspconfig", event = "BufEnter" }, -- config lsp servers
	{ "glepnir/lspsaga.nvim", branch = "main", event = "BufEnter" },
	{
		"williamboman/mason.nvim",
		event = "BufEnter",
		build = ":MasonUpdate", -- updates registry contents
	},
	{ "williamboman/mason-lspconfig.nvim", event = "BufEnter" }, -- bridges mason.nvim with the nvim-lspconfig
	{ "jose-elias-alvarez/typescript.nvim", ft = { "typescript", "typescriptreact", "javascript", "javascriptreact" } },
	{ "b0o/schemastore.nvim", ft = { "json" } },
	{ "jose-elias-alvarez/null-ls.nvim", event = "BufEnter" }, -- for formatters and linters
	{
		"MunifTanjim/prettier.nvim",
		ft = { "html", "css", "scss", "javascript", "javascriptreact", "typescript", "typescriptreact", "yaml", "json" },
		config = function()
			require("plugins.prettier")
		end,
	},
	-- {
	-- 	"elixir-tools/elixir-tools.nvim",
	-- 	ft = { "elixir" },
	-- 	version = "*",
	-- 	event = { "BufReadPre", "BufNewFile" },
	-- 	config = function()
	-- 		require("plugins.elixir-tools")
	-- 	end,
	-- 	dependencies = {
	-- 		"nvim-lua/plenary.nvim",
	-- 	},
	-- },
	{ "fatih/vim-go", ft = { "go" } },
	{
		"simrat39/rust-tools.nvim",
		ft = { "rust" },
		config = function()
			require("plugins.rust-tools")
		end,
	},

	--[[ DAP ]]
	{
		"mfussenegger/nvim-dap",
		keys = {
			{ "<leader>db", ":DapToggleBreakpoint<cr>", mode = { "n" } },
		},
	},
	{
		"rcarriga/nvim-dap-ui",
		dependencies = "mfussenegger/nvim-dap",
		config = function()
			require("plugins.dapui")
		end,
	},
	{
		"mfussenegger/nvim-dap-python",
		ft = "python",
		keys = {
			{ "<localleader>dr", ":lua require('dap-python').test_method()<cr>", mode = { "n" } },
		},
		dependencies = { "nvim-dap", "rcarriga/nvim-dap-ui" },
		config = function(_, opts)
			local path = "~/.local/share/nvim/mason/packages/debugpy/venv/bin/python "
			require("dap-python").setup(path)
		end,
	},

	{
		"ThePrimeagen/harpoon",
		config = function()
			require("plugins.harpoon")
		end,
	},

	--[[ Code Completion ]]
	-- snippets
	{
		"rafamadriz/friendly-snippets",
		event = "InsertEnter",
	},
	-- {
	-- 	"f-person/git-blame.nvim",
	-- 	keys = {
	-- 		{ "<leader>gb", ":GitBlameToggle<cr>", mode = { "n" } },
	-- 	},
	-- 	config = function()
	-- 		vim.cmd([[
	--      let g:gitblame_enabled = 0
	--      let g:gitblame_message_template = '<author>  <summary>   <date>'
	--      ]])
	-- 	end,
	-- },
	{
		"hrsh7th/nvim-cmp", -- cmp plugins
		event = "InsertEnter",
		config = function()
			require("plugins.cmp")
		end,
		dependencies = {
			"saadparwaiz1/cmp_luasnip", -- snippet completions
			"hrsh7th/cmp-buffer", -- buffer completions
			"hrsh7th/cmp-path", -- path completions
			"hrsh7th/cmp-cmdline", -- cmdline completions
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-nvim-lua", -- for neovim Lua API
			-- "hrsh7th/cmp-emoji",
			"onsails/lspkind.nvim",
			"petertriho/cmp-git",
		},
	},
	{
		"L3MON4D3/LuaSnip",
		version = "2.*",
		event = "InsertEnter",
		config = function()
			require("plugins.luasnip")
		end,
		dependencies = { "rafamadriz/friendly-snippets" },
		build = "make install_jsregexp",
	}, --snippet engine

	{
		-- 🌐  https://github.com/Olical/conjure/wiki/
		"Olical/conjure",
		ft = conjure_filetypes,
		keys = {
			{ ",cb", ":ConjureEvalBuf<cr>", mode = { "n" } },
			{ ",cp", ":ConjureEvalCurrentForm<cr>", mode = { "n" } },
			{ ",cr", ":ConjureEvalRootForm<cr>", mode = { "n" } },
			{ ",cc", ":ConjureEvalCommentCurrentForm<cr>", mode = { "n" } },
			{ ",cC", ":ConjureEvalCommentRootForm<cr>", mode = { "n" } },
			{ ",cv", ":'<,'>ConjureEvalVisual<cr>", mode = { "v" } },
			{ ",cw", ":ConjureEvalWord<cr>", mode = { "n" } },
		},
	},
	"tpope/vim-dispatch",
	"clojure-vim/vim-jack-in",
	-- ## lisp
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
	{ "gpanders/nvim-parinfer", ft = lisp_filetypes },
	{ "guns/vim-sexp", ft = lisp_filetypes },
	{ "tpope/vim-sexp-mappings-for-regular-people", ft = lisp_filetypes },
	-- use("Olical/conjure")
	-- use("vlime/vlime")
	--[[\cc: create a server connection.
        \cs: choose a server connection.
        \ss: send the current line to REPL for evaluation.
        \i: toggle interactive mode (in interactive mode, press <CR> will execute the code)
      ]]

	-- ## git
	{
		"lewis6991/gitsigns.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.gitsigns")
		end,
	},
	{
		"sindrets/diffview.nvim",
		keys = {
			{ "<leader>gg", ":DiffviewOpen<cr>", mode = { "n", "v" } },
			{ "<leader>g ", ":DiffviewClose<cr>", mode = { "n", "v" } },
		},
		config = function()
			require("plugins.gitsigns")
		end,
	},
	-- ## Telescope
	-- run `make` inside `telescope-fzf-native` plugin directory
	{ "nvim-telescope/telescope-fzf-native.nvim", build = "make", lazy = true },
	{
		"nvim-telescope/telescope.nvim",
		event = "BufEnter",
		-- keys = {
		-- 	{ "<leader>f ", ":Telescope find_files<cr>" },
		-- 	{ "<leader>f.", ":Telescope live_grep<cr>" },
		-- 	{ "<leader>ff", ":Telescope find_files hidden=true<cr>" },
		-- },
		config = function()
			require("plugins.telescope")
		end,
	},

	-- ## Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		lazy = true,
		build = ":TSUpdate",
		config = function()
			require("plugins.treesitter")
		end,
		dependencies = {
			"HiPhish/nvim-ts-rainbow2",
			"nvim-treesitter/playground",
			"windwp/nvim-ts-autotag",
			{
				"nvim-treesitter/nvim-treesitter-context",
				config = function()
					require("plugins.treesitter-context")
				end,
			},
			{
				"nvim-treesitter/nvim-treesitter-textobjects",
			},
		},
	},
	{
		"laytan/tailwind-sorter.nvim",
		dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-lua/plenary.nvim" },
		build = "cd formatter && npm i && npm run build",
		config = true,
	},
	{
		"nvim-orgmode/orgmode",
		config = function()
			require("plugins.orgmode")
		end,
	},
	{
		"lukas-reineke/indent-blankline.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.indent")
		end,
	},
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 500
		end,
		opts = {
			-- your configuration comes here
			-- or leave it empty to use the default settings
			-- refer to the configuration section below
		},
		config = function()
			require("plugins.which-key")
		end,
	},
	{
		"jackMort/ChatGPT.nvim",
		event = "VeryLazy",
		commit = "24bcca7",
		keys = {
			{ "<leader>pc", ":ChatGPT<cr>", mode = { "n", "v" } },
			{ "<leader>pC", ":ChatGPT<cr>:ChatGPT<cr><C-w><C-w>", mode = { "n", "v" } },
			{ "<leader>p", ":ChatGPT<cr>:ChatGPT<cr>", mode = { "n", "v" } },
			{ "<leader>pa", ":ChatGPTActAs<cr>", mode = { "n", "v" } },
			{ "<leader>ac", ":ChatGPTCompleteCode<cr>", mode = { "n", "v" } },
			{ "<leader>ai", ":ChatGPTEditWithInstructions<cr>", mode = { "n", "v" } },
		},
		config = function()
			require("plugins.chatgpt")
		end,
		dependencies = {
			"MunifTanjim/nui.nvim",
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
		},
	},
	{
		"norcalli/nvim-colorizer.lua",
		event = "BufEnter",
		config = function()
			require("colorizer").setup()
		end,
	},
	{
		"folke/twilight.nvim",
		keys = { { "<leader>zt", ":Twilight<cr>" } },
		config = function()
			require("plugins.twilight")
		end,
	},
	{
		"folke/zen-mode.nvim",
		keys = { { "<leader>zz", ":ZenMode<cr>" } },
		config = function()
			require("plugins.zen-mode")
		end,
	},
	{
		"gelguy/wilder.nvim",
		keys = { { ":", mode = "n" }, { "/", mode = "n" }, { "?", mode = "n" } },
		config = function()
			require("plugins.wilder-menu")
		end,
	},
	{
		"anuvyklack/hydra.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.hydra")
		end,
	}, -- submodes and menus
	{
		"ellisonleao/glow.nvim",
		cmd = "Glow",
		config = function()
			require("plugins.glow")
		end,
	}, -- markdown preview
	{
		"glacambre/firenvim",
		cond = not not vim.g.started_by_firenvim,
		build = function()
			require("lazy").load({ plugins = "firenvim", wait = true })
			vim.fn["firenvim#install"](0)
		end,
		config = function()
			require("plugins.firenvim")
		end,
	},
	{
		"glepnir/dashboard-nvim",
		event = "VimEnter",
		config = function()
			require("plugins.dashboard")
		end,
		dependencies = { { "nvim-tree/nvim-web-devicons" } },
	},
}

local options = {
	install = { missing = true, colorscheme = { COLORSCHEME } },
	checker = { enabled = true, notify = true },
	change_detection = {
		enabled = true,
		notify = true,
	},
	performance = {
		cache = {
			enabled = true,
			reset_packpath = true,
		},
	},
	ui = {
		-- a number <1 is a percentage., >1 is a fixed size
		size = { width = 0.8, height = 0.8 },
		wrap = true, -- wrap the lines in the ui
		-- The border to use for the UI window. Accepts same border values as |nvim_open_win()|.
		border = "none",
		icons = {
			cmd = " ",
			config = "",
			event = "",
			ft = " ",
			init = " ",
			import = " ",
			keys = " ",
			lazy = "∾ ",
			loaded = "●",
			not_loaded = "○",
			plugin = " ",
			runtime = " ",
			source = " ",
			start = "",
			task = "✔ ",
			list = {
				"●",
				"➜",
				"★",
				"‒",
			},
		},
	},
}

lazy.setup(plugins, options)
