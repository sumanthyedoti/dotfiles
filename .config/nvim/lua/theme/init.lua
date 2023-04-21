-- taransparent BG
vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

-- HERE: colorscheme
require("theme.catppuccin")
local colorscheme = "catppuccin-mocha"
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
