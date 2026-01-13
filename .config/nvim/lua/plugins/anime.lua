return {
  dir = "~/projects/anime.nvim",
  dependencies = { "nvim-lualine/lualine.nvim" },
  config = function()
    require("anime").setup()
    -- require("lualine").setup({
    --   sections = {
    --     lualine_x = {
    --       function()
    --         return require("anime").get_status()
    --       end,
    --       "encoding",
    --       "fileformat",
    --       "filetype",
    --     },
    --   },
    -- })
  end,
}
