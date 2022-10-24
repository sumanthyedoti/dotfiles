local status_ok, dash = pcall(require, "dashboard")
if not status_ok then
  return
end

dash.custom_header = {
  '⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣀⣀⣀⣀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀',
  '⠀⠀⠀⠀⠀⠀⠀⢀⣠⠶⠞⢛⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣶⣄⡀⠀⠀⠀⠀⠀⠀⠀',
  '⠀⠀⠀⠀⠀⣠⡾⠋⠀⠀⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣄⠀⠀⠀⠀⠀',
  '⠀⠀⠀⢀⡾⠋⠀⠀⠀⠀⢰⣿⣿⣿⡿⠉⠀⠀⠀⠉⢿⣿⣿⣿⣿⣿⣿⣿⣷⡀⠀⠀⠀',
  '⠀⠀⢠⡞⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣇⠀⠀⠀⠀⠀⣸⣿⣿⣿⣿⣿⣿⣿⣿⣷⡄⠀⠀',
  '⠀⠀⣾⠁⠀⠀⠀⠀⠀⠀⠘⣿⣿⣿⣿⣷⣤⣤⣤⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀',
  '⠀⢸⡏⠀⠀⠀⠀⠀⠀⠀⠀⠘⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀',
  '⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠙⠛⠛⠻⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀',
  '⠀⢸⣇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀',
  '⠀⠀⢿⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣤⣤⣤⡀⠀⠀⠀⠀⢻⣿⣿⣿⣿⣿⣿⣿⡿⠀⠀',
  '⠀⠀⠘⢧⠀⠀⠀⠀⠀⠀⠀⠀⢰⣿⣿⣿⣿⣿⡆⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⡿⠃⠀⠀',
  '⠀⠀⠀⠈⢷⣄⠀⠀⠀⠀⠀⠀⠈⠿⣿⣿⣿⠿⠁⠀⠀⠀⣸⣿⣿⣿⣿⣿⡿⠁⠀⠀⠀',
  '⠀⠀⠀⠀⠀⠙⢷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⣿⣿⣿⡿⠋⠀⠀⠀⠀⠀',
  '⠀⠀⠀⠀⠀⠀⠀⠈⠙⠶⢦⣤⣄⣀⣀⣀⣀⣠⣤⣾⡿⠿⠿⠋⠁⠀⠀⠀⠀⠀⠀⠀',
  '⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠉⠉⠉⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀',
}
dash.custom_center = {
  { icon = '  ',
    desc = 'Explorer',
    action = 'NvimTreeOpen',
  },
  { icon = ' ',
    desc = 'Find File',
    action = 'Telescope find_files find_command=rg,--hidden,--files',
  },
  { icon = ' ',
    desc = 'Find word',
    action = 'Telescope live_grep',
  },
}

local quotes = {
  "Alasyam.. Amrutham! Visham!!",
}

dash.custom_footer = { quotes[math.random(1, #quotes)] }
