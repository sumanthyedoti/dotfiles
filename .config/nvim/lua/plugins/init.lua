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

vim.g.mapleader = " "
vim.g.maplocalleader = "z"

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
  firenvim
  dispatch
  neogit
  neo-minimap  - https://youtu.be/vNyQBWfSh7c
  colortils
    color-picker.nvim
  syntax-tree-surfer
  harpoon
  andweeb/presence.nvim (Discord)
  Wansmer/treesj
  which-key
  dail.nvim
  doom.nvim
  nvim-dap (mason)
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
  prettier.nvim
  vim-dadbod (for sql) [https://youtu.be/_DnmphIwnjo?t=980]
  hologram.nvim
  sleuth.vim
  nvim-notify
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
  - Neorg
  - nvim-orgmode
  - telekasten.nvim
  - Obsidian.nvim
  michaelb/sniprun *
  codi.vim
  vim-slime
  TODO:
  - configure gitsigns
  - configure hydra
]]

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
		vent = "BufEnter",
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
			{ "<C-y>", "<cmd>:ToggleTerm", mode = { "n", "t" } },
			{ "<leader>tt", "<cmd>Lspsaga term_toggle<CR>", mode = { "n", "t" } },
			{ "<leader>tg", "<cmd>lua _LAZYGIT_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>th", "<cmd>lua _HTOP_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>td", "<cmd>lua _NCDU_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>tn", "<cmd>lua _NODE_TOGGLE()<CR>", mode = { "n", "t" } },
			{ "<leader>tp", "<cmd>lua _PYTHON_TOGGLE()<CR>", mode = { "n", "t" } },
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
	{ "fatih/vim-go", ft = { "go" } },
	{
		"simrat39/rust-tools.nvim",
		ft = { "rust" },
		config = function()
			require("plugins.rust-tools")
		end,
	},

	--[[ Code Completion ]]
	-- snippets
	{
		"rafamadriz/friendly-snippets",
		event = "InsertEnter",
	},
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
			"onsails/lspkind.nvim",
			"petertriho/cmp-git",
		},
	},
	{
		"L3MON4D3/LuaSnip",
		version = "1.*", -- follow latest release.
		event = "InsertEnter",
		config = function()
			require("plugins.luasnip")
		end,
		-- install jsregexp (optional!).
		build = "make install_jsregexp",
	}, --snippet engine

	--[[ Debugging ]]
	-- use("mfussenegger/nvim-dap")
	-- use({ "rcarriga/nvim-dap-ui", dependencies = { "mfussenegger/nvim-dap" }, config = "require 'plugins.dapui'" })

	-- ## lisp
	{ "jpalardy/vim-slime", ft = { "lisp", "lsp" } },
	{ "gpanders/nvim-parinfer", ft = { "lisp", "lsp" } },
	{ "guns/vim-sexp", ft = { "lisp", "lsp" } },
	{ "tpope/vim-sexp-mappings-for-regular-people", ft = { "lisp", "lsp" } },
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
		"apzelos/blamer.nvim",
		event = "VeryLazy",
		config = function()
			vim.g.blamer_enabled = 1
			vim.g.blamer_delay = 500
			vim.g.blamer_show_in_insert_modes = 0
			vim.g.blamer_prefix = "  "
			vim.g.blamer_show_in_visual_modes = 0
		end,
	},

	-- ## Telescope
	-- run `make` inside `telescope-fzf-native` plugin directory
	{ "nvim-telescope/telescope-fzf-native.nvim", build = "make", lazy = true },
	{
		"nvim-telescope/telescope.nvim",
		event = "BufEnter",
		-- keys = {
		-- 	{ "<leader>f ", "<cmd>Telescope find_files<cr>" },
		-- 	{ "<leader>f.", "<cmd>Telescope live_grep<cr>" },
		-- 	{ "<leader>ff", "<cmd>Telescope find_files hidden=true<cr>" },
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
			"p00f/nvim-ts-rainbow",
			"nvim-treesitter/playground",
			"windwp/nvim-ts-autotag",
			{
				"nvim-treesitter/nvim-treesitter-context",
				config = function()
					require("plugins.treesitter-context")
				end,
			},
		},
	},

	{
		"lukas-reineke/indent-blankline.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.indent")
		end,
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
		keys = { { "<leader>zt", ":Twilight" } },
		config = function()
			require("plugins.twilight")
		end,
	},
	{
		"folke/zen-mode.nvim",
		keys = { { "<leader>zz", ":ZenMode" } },
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
		"glepnir/dashboard-nvim",
		event = "VimEnter",
		config = function()
			require("plugins.dashboard")
		end,
		dependencies = { { "nvim-tree/nvim-web-devicons" } },
	},
}

local options = {
	install = { missing = true, colorscheme = { "tokyonight" } },
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
