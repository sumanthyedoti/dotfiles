return {
  'glepnir/dashboard-nvim',
  event = 'VimEnter',
  config = function()
    local dashboard = require("dashboard")
    local quotes = {
      "Alasyam.. Amrutham! Visham!!",
      "Start creating..",
      "Delete some code",
      "ABC -- Always Be Coding",
    }

    local function desc(content)
      return string.format("%-20s", content)
    end
    dashboard.setup({
      theme = "doom",
      config = {
        header = {
          "",
          "",
          "",
          "",
          "",
          "",
          -- "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣀⣀⣀⣀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
          -- "⠀⠀⠀⠀⠀⠀⠀⢀⣠⠶⠞⢛⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣶⣄⡀⠀⠀⠀⠀⠀⠀⠀",
          -- "⠀⠀⠀⠀⠀⣠⡾⠋⠀⠀⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣄⠀⠀⠀⠀⠀",
          -- "⠀⠀⠀⢀⡾⠋⠀⠀⠀⠀⢰⣿⣿⣿⡿⠉⠀⠀⠀⠉⢿⣿⣿⣿⣿⣿⣿⣿⣷⡀⠀⠀⠀",
          -- "⠀⠀⢠⡞⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣇⠀⠀⠀⠀⠀⣸⣿⣿⣿⣿⣿⣿⣿⣿⣷⡄⠀⠀",
          -- "⠀⠀⣾⠁⠀⠀⠀⠀⠀⠀⠘⣿⣿⣿⣿⣷⣤⣤⣤⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀",
          -- "⠀⢸⡏⠀⠀⠀⠀⠀⠀⠀⠀⠘⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀",
          -- "⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠙⠛⠛⠻⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀",
          -- "⠀⢸⣇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀",
          -- "⠀⠀⢿⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣤⣤⣤⡀⠀⠀⠀⠀⢻⣿⣿⣿⣿⣿⣿⣿⡿⠀⠀",
          -- "⠀⠀⠘⢧⠀⠀⠀⠀⠀⠀⠀⠀⢰⣿⣿⣿⣿⣿⡆⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⡿⠃⠀⠀",
          -- "⠀⠀⠀⠈⢷⣄⠀⠀⠀⠀⠀⠀⠈⠿⣿⣿⣿⠿⠁⠀⠀⠀⣸⣿⣿⣿⣿⣿⡿⠁⠀⠀⠀",
          -- "⠀⠀⠀⠀⠀⠙⢷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⣿⣿⣿⡿⠋⠀⠀⠀⠀⠀",
          -- lo
          -- "⠀⠀⠀⠀⠀⠀⠀⠈⠙⠶⢦⣤⣄⣀⣀⣀⣀⣠⣤⣾⡿⠿⠿⠋⠁⠀⠀⠀⠀⠀⠀⠀",
          -- "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠉⠉⠉⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
          "÷------------------------------------------------÷",
          "│                                                │",
          "│                                                │",
          "│        ( < neovim {} sumanthyedoti /> )        │",
          "│                                                │",
          "│                                                │",
          "÷------------------------------------------------÷",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
        },
        center = {
          {
            icon = " ",
            desc = desc("Find File"),
            key = "f",
            action = "Telescope find_files find_command=rg,--files",
          },
          {
            icon = " ",
            desc = desc("Find Word"),
            key = "w",
            action = "Telescope live_grep",
          },
          { icon = " ", desc = desc("File Explorer"), key = "e", action = "NvimTreeOpen" },
          {
            icon = "﬒ ",
            desc = desc("Find Hidden File"),
            key = ".",
            action = "Telescope find_files find_command=rg,--hidden,--files",
          },
          {
            icon = " ",
            desc = " ",
            key = " ",
            action = " ",
          },
        },
        footer = { quotes[math.random(1, #quotes)] },
      },
    })
  end,
  dependencies = { {'nvim-tree/nvim-web-devicons'}}
}
