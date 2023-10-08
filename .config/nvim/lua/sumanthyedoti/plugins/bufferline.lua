return {
  "akinsho/bufferline.nvim",
  event = { "BufReadPre", "BufNewFile" },
  version = "*",
  dependencies = "nvim-tree/nvim-web-devicons",
  opts = { -- require("bufferline").setup({...opts})
    options = {
      mode = "buffers",
      separator_style = "slant", -- "slant" | "slope" | "thick" | "thin"
      offsets = { { filetype = "NvimTree", text = "", padding = 1 } },
      max_name_length = 30,
      max_prefix_length = 30, -- prefix used when a buffer is de-duplicated
      tab_size = 24,
      left_trunc_marker = "",
      right_trunc_marker = "",
    },
  },
}
