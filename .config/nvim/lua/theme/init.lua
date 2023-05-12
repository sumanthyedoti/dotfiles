-- HERE: colorscheme
require("theme.catppuccin")
local colorscheme = "tokyonight-night"
-- "gruvbox", "nightfly", "tokyonight-night" "onedark"
--[[ Nightfox
  dark: "nightfox", "duskfox", "nordfox", "terafox", "carbonfox"
  light: "dayfox", "dawnfox" ]]
-- [[ Catppuccin
-- catppuccin-latte, catppuccin-frappe, catppuccin-macchiato, catppuccin-mocha
-- ]]

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
	vim.notify("colorscheme " .. colorscheme .. " not found!")
	return
end
