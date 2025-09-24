return {
  "folke/noice.nvim",
  opts = function(_, opts)
    opts.presets = {
      command_palette = {
        views = {
          cmdline_popup = {
            position = {
              row = "85%",
              col = "50%",
            },
            size = {
              min_width = 60,
              width = "auto",
              height = "auto",
            },
          },
          cmdline_popupmenu = {
            position = {
              row = "75%",
              col = "50%",
            },
          },
        },
      },
    }
    opts.lsp.signature = {
      opts = { size = { max_height = 15 } },
    }
  end,
}
