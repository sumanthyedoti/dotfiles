-- following options are the default
-- each of these are documented in `:help nvim-tree.OPTION_NAME`

local status_ok, nvim_tree = pcall(require, "nvim-tree")
if not status_ok then
	return
end

local config_status_ok, nvim_tree_config = pcall(require, "nvim-tree.config")
if not config_status_ok then
	return
end

local tree_cb = nvim_tree_config.nvim_tree_callback

-- close NvimTree if last buffer is closed
vim.api.nvim_create_autocmd("BufEnter", {
	command = "if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif",
	nested = true,
})

nvim_tree.setup({
	filters = {
		dotfiles = false,
		custom = {},
		exclude = {},
	},
	disable_netrw = true,
	hijack_netrw = true,
	open_on_setup = false,
	ignore_ft_on_setup = {
		"startify",
		"dashboard",
		"alpha",
	},
	open_on_tab = false,
	hijack_cursor = false,
	update_cwd = true,
	hijack_directories = {
		enable = true,
		auto_open = true,
	},
	diagnostics = {
		enable = true,
		icons = {
			hint = "",
			info = "",
			warning = "",
			error = "",
		},
	},
	update_focused_file = {
		enable = true,
		update_cwd = true,
		ignore_list = {},
	},
	git = {
		enable = true,
		ignore = true,
		timeout = 500,
	},
	view = {
		hide_root_folder = false,
		side = "left",
		adaptive_size = true,
		mappings = {
			custom_only = false,
			list = {
				{
					key = "<S-v>",
					action = "split_right",
					action_cb = function(node)
						vim.cmd("vsplit" .. vim.fn.fnameescape(node.absolute_path))
					end,
				},
				{ key = { "<CR>", "o" }, cb = tree_cb("edit") },
				{ key = "<C-x>", cb = tree_cb("cut") },
				{ key = "<C-c>", cb = tree_cb("copy") },
				{ key = "<C-v>", cb = tree_cb("paste") },
				{ key = "s", cb = tree_cb("vsplit") },
				{ key = "S", cb = tree_cb("split") },
				{
					key = "<C-t>",
					action = "open in tab",
					action_cb = function(node)
						vim.cmd("tabnew" .. vim.fn.fnameescape(node.absolute_path))
						-- nvim_tree.toggle(false, true)
					end,
				},
				{ key = "D", cb = tree_cb("remove") },
			},
		},
		number = false,
		relativenumber = true,
	},
	renderer = {
		highlight_git = true,
		root_folder_modifier = ":t",
		icons = {
			show = {
				file = true,
				folder = true,
				folder_arrow = true,
				git = true,
			},
			glyphs = {
				default = "",
				symlink = "",
				git = {
					unstaged = "",
					staged = "S",
					unmerged = "",
					renamed = "➜",
					deleted = "",
					untracked = "U",
					ignored = "◌",
				},
				folder = {
					default = "",
					open = "",
					empty = "",
					empty_open = "",
					symlink = "",
				},
			},
		},
	},
})
