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
end

vim.api.nvim_create_user_command("FirenvimExpand", expand_firenvim, {})
