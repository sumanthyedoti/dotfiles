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
	--[[ LSP ]]
	{ "fatih/vim-go", ft = { "go" } },

	{
		"ThePrimeagen/harpoon",
		config = function()
			require("plugins.harpoon")
		end,
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

}

local options = {
	install = { missing = true, colorscheme = { COLORSCHEME } },
	checker = { enabled = true, notify = false },
	change_detection = {
		enabled = true,
		notify = false,
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

lazy.setup({
  {import = "plugins"},
  {import = "lsp"},
}, options)
