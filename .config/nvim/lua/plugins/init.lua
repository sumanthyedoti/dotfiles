local fn = vim.fn
-- Automatically install lazy
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
	print("Installing packer, close and reopen Neovim...")
end
vim.opt.rtp:prepend(lazypath)

-- Use a protected call so we don't error out on first use
local status_ok, lazy = pcall(require, "lazy")
if not status_ok then
	return
end

--########################################
-- Install your plugins here
--  packages are stored in '~/.local/share/nvim/site/pack/packer/start'
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
  sleuth.vim
  nvim-notify
  overseer.nvim
  GitHub Copilot
]]
lazy.setup({
	"wbthomason/packer.nvim", -- Have packer manage itself
	"nvim-lua/plenary.nvim", -- lua utility functions used by lots of plugins
	"nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
	"stevearc/dressing.nvim",
	{
		"windwp/nvim-autopairs",
		config = function()
			require("plugins.autopairs")
		end,
	},
	"mattn/webapi-vim",
	"tpope/vim-surround",
	"andymass/vim-matchup",
	{
		"ggandor/leap.nvim",
		config = function()
			require("plugins.leap")
		end,
	},
	{
		"AndrewRadev/switch.vim",
		config = function()
			require("plugins.switch")
		end,
	},
	{
		"iamcco/markdown-preview.nvim",
		run = function()
			vim.fn["mkdp#util#install"]()
		end,
	},
	"nvim-tree/nvim-web-devicons",
	{
		"nvim-lualine/lualine.nvim",
		config = function()
			require("plugins.lualine")
		end,
	}, --snippet engine
	-- colorschemes
	"EdenEast/nightfox.nvim",
	"ellisonleao/gruvbox.nvim",
	"folke/tokyonight.nvim", -- colorscheme
	"bluz71/vim-nightfly-guicolors",
	"navarasu/onedark.nvim",
	{
		"ziontee113/icon-picker.nvim",
		config = function()
			require("icon-picker").setup({
				disable_legacy_commands = true,
			})
		end,
	},
	{
		"folke/todo-comments.nvim",
		config = function()
			require("plugins.todo-comments")
		end,
	},
	{
		"akinsho/bufferline.nvim",
		config = function()
			require("plugins.bufferline")
		end,
	},
	{
		"akinsho/toggleterm.nvim",
		config = function()
			require("plugins.toggleterm")
		end,
	},
	"moll/vim-bbye", -- to close buffers
	{
		"weilbith/nvim-code-action-menu",
		cmd = "CodeActionMenu",
	},
	-- colorscheme
	-- ## Commentery
	"JoosepAlviste/nvim-ts-context-commentstring",
	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	},

	-- ## NVim Tree file explorer
	{
		"nvim-tree/nvim-tree.lua",
		dependencies = {
			"nvim-tree/nvim-web-devicons", -- optional, for file icons
		},
		tag = "nightly", -- optional, updated every week. (see issue #1193)
		config = function()
			require("plugins.nvim-tree")
		end,
	},

	-- ## Code Conpletion
	-- snippets
	{
		"L3MON4D3/LuaSnip",
		config = function()
			require("plugins.luasnip")
		end,
	}, --snippet engine
	"rafamadriz/friendly-snippets",
	-- cmp plugins
	{
		"hrsh7th/nvim-cmp",
		config = function()
			require("plugins.cmp")
		end,
		dependencies = { "L3MON4D3/LuaSnip" },
	},
	"saadparwaiz1/cmp_luasnip", -- snippet completions
	"hrsh7th/cmp-buffer", -- buffer completions
	"hrsh7th/cmp-path", -- path completions
	"hrsh7th/cmp-cmdline", -- cmdline completions
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-nvim-lua", -- for neovim Lua API
	"hrsh7th/vim-vsnip",
	"hrsh7th/cmp-vsnip",
	"onsails/lspkind.nvim",
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
	-- use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" }, config = "require 'plugins.dapui'" })

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
	{ "lewis6991/gitsigns.nvim" },
	{
		"apzelos/blamer.nvim",
		config = function()
			vim.g.blamer_enabled = 1
			vim.g.blamer_delay = 500
			vim.g.blamer_show_in_insert_modes = 0
			vim.g.blamer_prefix = "  "
			vim.g.blamer_show_in_visual_modes = 0
		end,
	},

	-- ## Telescope
	{ "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
	{
		"nvim-telescope/telescope.nvim",
		config = function()
			require("plugins.telescope")
		end,
	},

	-- ## Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		config = function()
			require("plugins.treesitter")
		end,
	},
	-- use({ "nvim-treesitter/nvim-treesitter-context", config = "require 'plugins.treesitter-context'" })
	"p00f/nvim-ts-rainbow",
	"nvim-treesitter/playground",
	"windwp/nvim-ts-autotag",

	{
		"lewis6991/impatient.nvim",
		config = function()
			require("impatient").enable_profile()
		end,
	},
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
		config = function()
			require("plugins.twilight")
		end,
	},
	{
		"folke/zen-mode.nvim",
		config = function()
			require("plugins.zen-mode")
		end,
	},
	{
		"gelguy/wilder.nvim",
		config = function()
			require("plugins.wilder-menu")
		end,
	},
	{
		"anuvyklack/hydra.nvim",
		config = function()
			require("plugins.hydra")
		end,
	}, -- submodes and menus
	{
		"ellisonleao/glow.nvim",
		config = function()
			require("plugins.glow")
		end,
	}, -- markdown preview
	"glepnir/dashboard-nvim",
})
