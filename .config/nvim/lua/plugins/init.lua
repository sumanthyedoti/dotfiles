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
  glow, without markdown-preview
  neogit
  neo-minimap  - https://youtu.be/vNyQBWfSh7c
  colortils
    color-picker.nvim
  syntax-tree-surfer
  neorg
  harpoon
  Wansmer/treesj
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
  vim-tmux-navigator
  vim-maximizer
  vim-illuminate
  prettier.nvim
  vim-dadbod
  hologram.nvim
  sleuth.vim
  nvim-notify
  overseer.nvim
  GitHub Copilot
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
		keys = "<C-y>",
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

	-- LSP
	"neovim/nvim-lspconfig", -- config lsp servers
	{ "glepnir/lspsaga.nvim", branch = "main" },
	"williamboman/mason.nvim", -- language server installer
	"williamboman/mason-lspconfig.nvim", -- bridges mason.nvim with the nvim-lspconfig
	"jose-elias-alvarez/typescript.nvim",
	"b0o/schemastore.nvim",
	"jose-elias-alvarez/null-ls.nvim", -- for formatters and linters
	"jayp0521/mason-null-ls.nvim",
	{
		"MunifTanjim/prettier.nvim",
		ft = { "html", "css", "scss", "javascript", "javascriptreact", "typescript", "typescriptreact", "yaml", "json" },
		config = function()
			require("plugins.prettier")
		end,
	},
	{ "fatih/vim-go" },
	{
		"simrat39/rust-tools.nvim",
		config = function()
			require("plugins.rust-tools")
		end,
	},

	-- Debugging
	-- use("mfussenegger/nvim-dap")
	-- use({ "rcarriga/nvim-dap-ui", dependencies = { "mfussenegger/nvim-dap" }, config = "require 'plugins.dapui'" })

	-- ## Code Completion
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
			"hrsh7th/vim-vsnip",
			"hrsh7th/cmp-vsnip",
			"onsails/lspkind.nvim",
			"petertriho/cmp-git",
		},
	},
	{
		"L3MON4D3/LuaSnip",
		-- follow latest release.
		version = "1.*",
		config = function()
			require("plugins.luasnip")
		end,
		-- install jsregexp (optional!).
		build = "make install_jsregexp",
	}, --snippet engine

	-- ## lisp
	{ "jpalardy/vim-slime" },
	{ "gpanders/nvim-parinfer" },
	{ "guns/vim-sexp" },
	{ "tpope/vim-sexp-mappings-for-regular-people" },
	-- use("Olical/conjure")
	-- use("vlime/vlime")
	--[[\cc: create a server connection.
        \cs: choose a server connection.
        \ss: send the current line to REPL for evaluation.
        \i: toggle interactive mode (in interactive mode, press <CR> will execute the code)
      ]]

	-- ## git
	{ "lewis6991/gitsigns.nvim", event = "BufEnter" },
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
	{ "nvim-telescope/telescope-fzf-native.nvim", build = "make", event = "BufEnter" },
	{
		"nvim-telescope/telescope.nvim",
		event = "BufEnter",
		config = function()
			require("plugins.telescope")
		end,
	},

	-- ## Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require("plugins.treesitter")
		end,
	},
	-- use({ "nvim-treesitter/nvim-treesitter-context", config = "require 'plugins.treesitter-context'" })
	"p00f/nvim-ts-rainbow",
	"nvim-treesitter/playground",
	"windwp/nvim-ts-autotag",

	{
		"lukas-reineke/indent-blankline.nvim",
		config = function()
			require("plugins.indent")
		end,
	},
	{
		"norcalli/nvim-colorizer.lua",
		config = function()
			require("colorizer").setup()
		end,
	},
	{
		"folke/twilight.nvim",
		keys = { "<leader>zt" },
		cmd = "Twilight",
		config = function()
			require("plugins.twilight")
		end,
	},
	{
		"folke/zen-mode.nvim",
		cmd = "ZenMode",
		keys = { "<leader>zz" },
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
}

lazy.setup(plugins, options)
