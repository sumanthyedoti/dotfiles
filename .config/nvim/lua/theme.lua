-- taransparent BG
vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

-- HERE: colorscheme
local colorscheme = "gruvbox" -- "gruvbox", "nightfly", "tokyonight-night" "onedark"
--[[ Nightfox
  dark: "nightfox", "duskfox", "nordfox", "terafox", "carbonfox"
  light: "dayfox", "dawnfox" ]]

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
	vim.notify("colorscheme " .. colorscheme .. " not found!")
	return
end
