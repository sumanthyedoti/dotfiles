return {
  "code-biscuits/nvim-biscuits",
  config = function()
    require("nvim-biscuits").setup({
      cursor_line_only = true,
      default_config = {
        max_length = 4,
        trim_by_words = true,
        min_distance = 5,
        prefix_string = " 🔃",
      },
      language_config = {
        html = {
          prefix_string = " 🌐 ",
        },
        javascript = {
          max_length = 80,
        },
        python = {
          disabled = false,
        },
      },
    })
  end,
}
