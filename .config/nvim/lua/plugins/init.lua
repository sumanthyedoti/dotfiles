local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  print "Installing packer close and reopen Neovim..."
  vim.cmd [[packadd packer.nvim]]
end

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Have packer use a popup window
packer.init {
  display = {
    open_fn = function()
      return require("packer.util").float { border = "rounded" }
    end,
  },
}

--########################################
-- Install your plugins here
--  packages are stored in '~/.local/share/nvim/site/pack/packer'
--[[ PACKAGES
  firenvim
  dispatch
  glow, without markdown-preview
  neogit
  hop.nvim or leap.nvim
  neo-minimap  - https://youtu.be/vNyQBWfSh7c
  colortils
    color-picker.nvim
  syntax-tree-surfer
  neorg
  anuvyklack/windows.nvim
  pretty-fold.nvim
  fold-preview.nvim
  cphealper
    CompetiTest.nvim
  alpha-nvim
    neovim-session-manager
  telescope-media-files
]]
return packer.startup(function(use)
  -- My plugins here
  use "wbthomason/packer.nvim" -- Have packer manage itself
  use "nvim-lua/plenary.nvim" -- lua utility functions used by lots of plugins
  use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
  use { "windwp/nvim-autopairs", config = function() require "plugins.autopairs" end }
  use "tpope/vim-surround"
  use "andymass/vim-matchup"
  use {
    "AndrewRadev/switch.vim",
    config = function()
      local opts = { noremap = true, silent = true }
      vim.api.nvim_set_keymap("n", "<leader>`", ":Switch<CR>", opts)
    end
  }
  use {
    "iamcco/markdown-preview.nvim",
    run = function() vim.fn["mkdp#util#install"]() end,
  }
  use 'nvim-tree/nvim-web-devicons'
  use 'nvim-lualine/lualine.nvim'
  use 'flazz/vim-colorschemes'
  use 'folke/tokyonight.nvim' -- colorscheme
  use { "stevearc/dressing.nvim", config = "require 'plugins.dressing'" } --snippet engine
  use {
    "ziontee113/icon-picker.nvim",
    config = function()
      require("icon-picker").setup({
        disable_legacy_commands = true
      })
    end,
  }
  use { '' }
  use { "akinsho/bufferline.nvim", config = "require 'plugins.bufferline'" } --snippet engine
  use { "akinsho/toggleterm.nvim", config = "require 'plugins.toggleterm'" } --snippet engine
  use 'moll/vim-bbye' -- to close buffers
  -- colorscheme
  -- ## Commentery
  use 'JoosepAlviste/nvim-ts-context-commentstring'
  use {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end
  }

  -- ## NVim Tree file explorer
  use {
    'nvim-tree/nvim-tree.lua',
    requires = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
    tag = 'nightly', -- optional, updated every week. (see issue #1193)
    config = "require 'plugins.nvim-tree'"
  }

  -- ## Code Conpletion
  -- snippets
  use { "L3MON4D3/LuaSnip", config = "require 'plugins.luasnip'" } --snippet engine
  use "rafamadriz/friendly-snippets"
  -- cmp plugins
  use { "hrsh7th/nvim-cmp", config = "require'plugins.cmp'", requires = { 'L3MON4D3/LuaSnip' } }
  use "saadparwaiz1/cmp_luasnip" -- snippet completions
  use "hrsh7th/cmp-buffer" -- buffer completions
  use "hrsh7th/cmp-path" -- path completions
  use "hrsh7th/cmp-cmdline" -- cmdline completions
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-nvim-lua" -- for neovim Lua API
  -- LSP
  use 'neovim/nvim-lspconfig'
  use "williamboman/mason.nvim" -- language server installer
  use "williamboman/mason-lspconfig.nvim" -- bridges mason.nvim with the nvim-lspconfig
  use "jose-elias-alvarez/null-ls.nvim" -- for formatters and linters


  -- ## git
  use { 'lewis6991/gitsigns.nvim' }

  -- ## Telescope
  use ""
  use { "nvim-telescope/telescope.nvim", config = "require 'plugins.telescope'" }

  -- ## Treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    config = "require 'plugins.treesitter'"
  }
  use "p00f/nvim-ts-rainbow"
  use "nvim-treesitter/playground"
  use "windwp/nvim-ts-autotag"

  use {
    'lewis6991/impatient.nvim',
    config = function()
      require('impatient').enable_profile()
    end
  }
  use { "lukas-reineke/indent-blankline.nvim", config = "require 'plugins.indent'" }
  use {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require 'colorizer'.setup()
    end
  }
  use { "folke/twilight.nvim", config = "require 'plugins.twilight'" }
  use { "folke/zen-mode.nvim", config = "require 'plugins.zen-mode'" }
  use { "gelguy/wilder.nvim", config = "require 'plugins.wilder-menu'" }
  use { "anuvyklack/hydra.nvim", config = "require 'plugins.hydra'" }
  use "glepnir/dashboard-nvim"

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
