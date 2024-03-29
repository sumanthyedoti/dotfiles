return {
  'rcarriga/nvim-notify',
  event = { 'BufReadPost', 'BufNewFile' },
  opts = {
    timeout = 10000,
    max_height = function()
      return math.floor(vim.o.lines * 0.75)
    end,
    max_width = function()
      return math.floor(vim.o.columns * 0.75)
    end,
  },
  config = function(_, opts)
    local notify = require('notify')
    notify.setup(opts)
    -- vim.notify = notify
  end
}
