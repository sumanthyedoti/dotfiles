-- HERE: colorscheme
require("sumanthyedoti.theme.catppuccin")

COLORSCHEME = "sonokai"
local colorscheme = COLORSCHEME
-- "gruvbox", "nightfly", "tokyonight-night" "onedark" "night-owl" "everforest"
--[[ Nightfox
  dark: "nightfox", "duskfox", "nordfox", "terafox", "carbonfox" "sonokai"
  light: "dayfox", "dawnfox" ]]
-- [[ Catppuccin
-- catppuccin-latte, catppuccin-frappe, catppuccin-macchiato, catppuccin-mocha
-- ]]

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
  vim.notify("colorscheme " .. colorscheme .. " not found!")
  return
end
