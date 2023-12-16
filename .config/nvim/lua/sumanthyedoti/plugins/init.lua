return {
  "nvim-lua/plenary.nvim", -- lua utility functions used by lots of plugins
  {
    "stevearc/dressing.nvim",
    opts = {},
  },
  {
    "norcalli/nvim-colorizer.lua",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("colorizer").setup()
    end,
  },
  {
    "laytan/tailwind-sorter.nvim",
    ft = { "html", "css", "javascriptreact", "typescriptreact" },
    dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-lua/plenary.nvim" },
    build = "cd formatter && npm i && npm run build",
    config = true,
  },
  {
    "mbbill/undotree",
    keys = {
      {
        "<leader>TU",
        ":UndotreeToggle<CR><C-w><C-w>:set number relativenumber<CR>",
        mode = { "n", "v" },
        desc = "UndoTree",
      },
    },
  },
  {
    "michaelb/sniprun",
    build = "sh ./install.sh",
    keys = {
      { "<leader>sr", ":SnipRun<cr>", mode = { "n", "v" }, desc = "Snip Run" },
      { "<leader>sR", ":SnipReset<cr>", mode = { "n", "v" }, desc = "Snip Reset" },
    },
  },
  { "christoomey/vim-tmux-navigator" },
  { "jbyuki/venn.nvim", cmd = "VBox" },

  --[[ vim plugins ]]
  { "tpope/vim-surround", event = { "BufReadPre", "BufNewFile" } },
  { "tpope/vim-repeat", event = { "BufReadPre", "BufNewFile" } },
  {
    "mattn/emmet-vim",
    ft = { "html", "css", "javascriptreact", "typescriptreact", "heex" },
    config = function()
      vim.cmd([[
      let g:user_emmet_leader_key='<C->'
      ]])
    end,
  },
  { "mg979/vim-visual-multi", event = { "BufReadPre", "BufNewFile" } },
  {
    "andymass/vim-matchup",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
    end,
  },
  {
    "moll/vim-bbye",
    keys = {
      { "<leader>bw", ":Bdelete<cr>", mode = { "n", "v" }, desc = "Delete current buffer" },
    },
  },
  {
    "dstein64/vim-startuptime",
    cmd = "StartupTime",
    init = function() -- init is called during startup. Configuration for vim plugins typically should be set in an init function
      vim.g.startuptime_tries = 6
    end,
  },
  { "mattn/webapi-vim", event = "BufEnter" },
  { "elixir-editors/vim-elixir", ft = { "elixir", "heex" } },
  { "mfussenegger/nvim-jdtls", ft = { "java" } },
}
