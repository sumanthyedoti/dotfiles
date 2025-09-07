-- markdown preview
return {
  "ellisonleao/glow.nvim",
  cmd = "Glow",
  config = function()
    local glow = require("glow")

    glow.setup({
      border = "shadow", -- floating window border config
      style = "dark", -- filled automatically with your current editor background, you can override using glow json style
      pager = false,
      width = 80,
    })

  end,
}
