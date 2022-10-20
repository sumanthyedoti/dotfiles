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

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

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
-- maybe
-- - firenvim
-- - dispatch
-- - glow, without markdown-preview
return packer.startup(function(use)
  -- My plugins here
  use "wbthomason/packer.nvim" -- Have packer manage itself
  use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
  use "nvim-lua/plenary.nvim" -- Useful lua functions used by lots of plugins
  use {
    "windwp/nvim-autopairs",
    config = function() require("nvim-autopairs").setup {} end
  }
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
  use { 'stevearc/dressing.nvim' }
  use {
    "ziontee113/icon-picker.nvim",
    config = function()
      require("icon-picker").setup({
        disable_legacy_commands = true
      })
    end,
  }
  use { 'akinsho/bufferline.nvim' }
  use 'moll/vim-bbye' -- to close buffers
  use "akinsho/toggleterm.nvim"
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
    tag = 'nightly' -- optional, updated every week. (see issue #1193)
  }

  -- ## Code Conpletion
  -- snippets
  use "L3MON4D3/LuaSnip" --snippet engine
  use "rafamadriz/friendly-snippets"
  -- cmp plugins
  use "hrsh7th/nvim-cmp"
  use "hrsh7th/cmp-buffer" -- buffer completions
  use "hrsh7th/cmp-path" -- path completions
  use "hrsh7th/cmp-cmdline" -- cmdline completions
  use "saadparwaiz1/cmp_luasnip" -- snippet completions
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-nvim-lua" -- for neovim Lua API

  -- ## git
  use { 'lewis6991/gitsigns.nvim' }

  -- ## LSP
  use 'neovim/nvim-lspconfig'
  use "williamboman/mason.nvim" -- language server installer
  use "williamboman/mason-lspconfig.nvim" -- bridges mason.nvim with the nvim-lspconfig
  use "jose-elias-alvarez/null-ls.nvim" -- for formatters and linters

  -- ## Telescope
  use "nvim-telescope/telescope.nvim"
  -- use "nvim-telescope/telescope-media-files.nvim"

  -- ## Treesitter
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
  }
  use "p00f/nvim-ts-rainbow"
  use "nvim-treesitter/playground"


  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
