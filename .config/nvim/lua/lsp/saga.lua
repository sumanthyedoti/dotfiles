local saga_status, saga = pcall(require, "lspsaga")
if not saga_status then
	return
end

saga.setup({
	-- keybinds for navigation in lspsaga window
	move_in_saga = { prev = "<C-k>", next = "<C-j>" },
	-- use enter to open file with finder
	finder = { -- lsp_finder
		-- max_height = 0.5,
		-- min_width = 30,
		-- force_max_height = false,
		keys = {
			jump_to = "p",
			expand_or_jump = "o",
			vsplit = "v",
			split = "x",
			tabe = "t",
			tabnew = "r",
			quit = { "q", "<ESC>" },
			close_in_preview = "<ESC>",
		},
	},
	definition = {
		edit = "o",
		vsplit = "v",
		split = "x",
		tabe = "t",
		quit = { "q", "<ESC>" },
	},
})
