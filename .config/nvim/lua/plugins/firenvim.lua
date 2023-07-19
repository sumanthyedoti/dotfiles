vim.g.firenvim_config = {
	globalSettings = { alt = "all" },
	localSettings = {
		[".*"] = {
			cmdline = "neovim",
			content = "text",
			priority = 0,
			selector = "textarea",
			takeover = "always",
		},
	},
}

local function expand_firenvim()
	vim.o.lines = 40
	vim.o.columns = 120
	vim.o.filetype = "markdown"
	vim.o.cmdheight = 1
	vim.o.noruler = true
	vim.o.noshowcmd = true
	vim.o.laststatus = 0
	vim.o.showtabline = 0
end

vim.api.nvim_create_user_command("FirenvimExpand", expand_firenvim, {})
