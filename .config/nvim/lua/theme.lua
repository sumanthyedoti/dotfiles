require("nightfox").setup({
	options = {
		-- Compiled file's destination location
		compile_path = vim.fn.stdpath("cache") .. "/nightfox",
		compile_file_suffix = "_compiled", -- Compiled file suffix
		transparent = false, -- Disable setting background
		terminal_colors = true, -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
		dim_inactive = false, -- Non focused panes set to alternative background
		--[[
    ## attr-list values
      bold
      underline
      undercurl
      underdouble
      underdotted
      underdashed
      strikethrough
      reverse
      inverse	--	same as reverse
      italic
      standout
      nocombine --	override attributes instead of combining them
      NONE
    ]]

		styles = { -- Style to be applied to different syntax groups
			comments = "italic", -- Value is any valid attr-list value `:help attr-list`
			conditionals = "NONE",
			constants = "NONE",
			functions = "NONE",
			keywords = "bold",
			numbers = "NONE",
			operators = "NONE",
			strings = "NONE",
			types = "italic,bold",
			variables = "NONE",
		},
		inverse = { -- Inverse highlight for different types
			match_paren = false,
			visual = false,
			search = false,
		},
		modules = { -- List of various plugins and additional options
			-- ...
		},
	},
	palettes = {},
	specs = {},
	groups = {},
})

-- HERE: colorscheme
local colorscheme = "tokyonight-night" -- "gruvbox", "nightfly", "tokyonight-night"
--[[ Nightfox
  dark: "nightfox", "duskfox", "nordfox", "terafox", "carbonfox"
  light: "dayfox", "dawnfox" ]]

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
	vim.notify("colorscheme " .. colorscheme .. " not found!")
	return
end
