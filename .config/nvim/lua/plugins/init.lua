local fn = vim.fn
-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	print("Installing packer, close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

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
return packer.startup(function(use)
	-- My plugins here
	use("wbthomason/packer.nvim") -- Have packer manage itself
	use("nvim-lua/plenary.nvim") -- lua utility functions used by lots of plugins
	use("nvim-lua/popup.nvim") -- An implementation of the Popup API from vim in Neovim
	use({ "stevearc/dressing.nvim" })
	use({ "liuchengxu/vim-clap" })
	use({
		"windwp/nvim-autopairs",
		config = function()
			require("plugins.autopairs")
		end,
	})
	use("mattn/webapi-vim")
	use("tpope/vim-surround")
	use("andymass/vim-matchup")
	use({ "ggandor/leap.nvim", config = "require 'plugins.leap'" })
	use({ "AndrewRadev/switch.vim", config = "require 'plugins.switch'" })
	use({
		"iamcco/markdown-preview.nvim",
		run = function()
			vim.fn["mkdp#util#install"]()
		end,
	})
	use("nvim-tree/nvim-web-devicons")
	use({ "nvim-lualine/lualine.nvim", config = "require 'plugins.lualine'" }) --snippet engine
	-- colorschemes
	use("EdenEast/nightfox.nvim")
	use({ "ellisonleao/gruvbox.nvim" })
	use("folke/tokyonight.nvim") -- colorscheme
	use("bluz71/vim-nightfly-guicolors")
	use("navarasu/onedark.nvim")
	use({
		"ziontee113/icon-picker.nvim",
		config = function()
			require("icon-picker").setup({
				disable_legacy_commands = true,
			})
		end,
	})
	use({ "folke/todo-comments.nvim", config = "require 'plugins.todo-comments'" })
	use({ "akinsho/bufferline.nvim", config = "require 'plugins.bufferline'" })
	use({ "akinsho/toggleterm.nvim", config = "require 'plugins.toggleterm'" })
	use("moll/vim-bbye") -- to close buffers
	use({
		"weilbith/nvim-code-action-menu",
		cmd = "CodeActionMenu",
	})
	-- colorscheme
	-- ## Commentery
	use("JoosepAlviste/nvim-ts-context-commentstring")
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	})

	-- ## NVim Tree file explorer
	use({
		"nvim-tree/nvim-tree.lua",
		requires = {
			"nvim-tree/nvim-web-devicons", -- optional, for file icons
		},
		tag = "nightly", -- optional, updated every week. (see issue #1193)
		config = "require 'plugins.nvim-tree'",
	})

	-- ## Code Conpletion
	-- snippets
	use({ "L3MON4D3/LuaSnip", config = "require 'plugins.luasnip'" }) --snippet engine
	use("rafamadriz/friendly-snippets")
	-- cmp plugins
	use({ "hrsh7th/nvim-cmp", config = "require'plugins.cmp'", requires = { "L3MON4D3/LuaSnip" } })
	use("saadparwaiz1/cmp_luasnip") -- snippet completions
	use("hrsh7th/cmp-buffer") -- buffer completions
	use("hrsh7th/cmp-path") -- path completions
	use("hrsh7th/cmp-cmdline") -- cmdline completions
	use("hrsh7th/cmp-nvim-lsp")
	use("hrsh7th/cmp-nvim-lua") -- for neovim Lua API
	use("hrsh7th/vim-vsnip")
	use("hrsh7th/cmp-vsnip")
	use("onsails/lspkind.nvim")
	-- LSP
	use("neovim/nvim-lspconfig") -- config lsp servers
	use({ "glepnir/lspsaga.nvim", branch = "main" })
	use("williamboman/mason.nvim") -- language server installer
	use("williamboman/mason-lspconfig.nvim") -- bridges mason.nvim with the nvim-lspconfig
	use("jose-elias-alvarez/typescript.nvim")
	use("b0o/schemastore.nvim")
	use("jose-elias-alvarez/null-ls.nvim") -- for formatters and linters
	use("jayp0521/mason-null-ls.nvim")
	use({ "MunifTanjim/prettier.nvim", config = "require 'plugins.prettier'" })
	use({ "fatih/vim-go" })
	use({ "simrat39/rust-tools.nvim", config = "require 'plugins.rust-tools'" })

	-- Debugging
	-- use("mfussenegger/nvim-dap")
	-- use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap" }, config = "require 'plugins.dapui'" })

	-- ## lisp
	use({ "jpalardy/vim-slime" })
	use({ "gpanders/nvim-parinfer" })
	use({ "guns/vim-sexp" })
	use({ "tpope/vim-sexp-mappings-for-regular-people" })
	-- use("Olical/conjure")
	-- use("vlime/vlime")
	--[[\cc: create a server connection.
        \cs: choose a server connection.
        \ss: send the current line to REPL for evaluation.
        \i: toggle interactive mode (in interactive mode, press <CR> will execute the code)
      ]]

	-- ## git
	use({ "lewis6991/gitsigns.nvim" })
	use({
		"apzelos/blamer.nvim",
		config = function()
			vim.g.blamer_enabled = 1
			vim.g.blamer_delay = 500
			vim.g.blamer_show_in_insert_modes = 0
			vim.g.blamer_prefix = "  "
		end,
	})

	-- ## Telescope
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
	use({ "nvim-telescope/telescope.nvim", config = "require 'plugins.telescope'" })

	-- ## Treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		config = "require 'plugins.treesitter'",
	})
	-- use({ "nvim-treesitter/nvim-treesitter-context", config = "require 'plugins.treesitter-context'" })
	use("p00f/nvim-ts-rainbow")
	use("nvim-treesitter/playground")
	use("windwp/nvim-ts-autotag")

	use({
		"lewis6991/impatient.nvim",
		config = function()
			require("impatient").enable_profile()
		end,
	})
	use({ "lukas-reineke/indent-blankline.nvim", config = "require 'plugins.indent'" })
	use({
		"norcalli/nvim-colorizer.lua",
		config = function()
			require("colorizer").setup()
		end,
	})
	use({ "folke/twilight.nvim", config = "require 'plugins.twilight'" })
	use({ "folke/zen-mode.nvim", config = "require 'plugins.zen-mode'" })
	use({ "gelguy/wilder.nvim", config = "require 'plugins.wilder-menu'" })
	use({ "anuvyklack/hydra.nvim", config = "require 'plugins.hydra'" }) -- submodes and menus
	use({ "ellisonleao/glow.nvim", config = "require 'plugins.glow'" }) -- markdown preview
	use("glepnir/dashboard-nvim")

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
