return {
  "ggandor/leap.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local leap = require("leap")
    leap.add_default_mappings()

    leap.opts = {}

    -- leap.add_repeat_mappings(';', ',', { -- False by default
    --   relative_directions = true,
    --   -- By default, all modes are included.
    --   modes = {'n', 'x', 'o'},
    -- })

    vim.cmd([[
    autocmd ColorScheme * lua require('leap').init_highlight(true)
    ]])
  end,
  dependencies = {
    "tpope/vim-repeat",
  },
}
