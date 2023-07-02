local ls_ok, ls = pcall(require, "luasnip")
if not ls_ok then
	vim.notify("luasnip load failed")
	return
end

require("luasnip.loaders.from_vscode").lazy_load()
require("luasnip.loaders.from_lua").load({ paths = "~/.config/nvim/lua/snippets/" })
require("snips")
require("luasnip").config.setup({ store_selection_keys = "<A-p>" })

vim.cmd([[command! LuaSnipEdit :lua require("luasnip.loaders.from_lua").edit_snippet_files()]]) --}}}

-- Virtual Text{{{
local types = require("luasnip.util.types")
ls.config.set_config({
	history = true, --keep around last snippet local to jump back
	updateevents = "TextChanged,TextChangedI", --update changes as you type
	enable_autosnippets = true,
	ext_opts = {
		[types.choiceNode] = {
			active = {
				virt_text = { { "", "GruvboxAqua" } },
			},
		},
		-- [types.insertNode] = {
		-- 	active = {
		-- 		virt_text = { { "●", "GruvboxBlue" } },
		-- 	},
		-- },
	},
})
-- Key Mapping --
local function reload_package(package_name)
	for module_name, _ in pairs(package.loaded) do
		if string.find(module_name, "^" .. package_name) then
			package.loaded[module_name] = nil
			require(module_name)
		end
	end
end

local function refresh_snippets()
	ls.cleanup()
	reload_package("snips")
end

local set = vim.keymap.set
local mode = { "i", "s" }
local normal = { "n" }
set(normal, ",r", refresh_snippets)
set(mode, "<a-k>", function()
	if ls.expand_or_jumpable() then
		ls.expand_or_jump()
	end
end, { silent = true })

set(mode, "<a-j>", function()
	if ls.jumpable() then
		ls.jump(-1)
	end
end, { silent = true })

set(mode, "<a-l>", function()
	if ls.choice_active() then
		ls.change_choice(-1)
	end
end)

-- set(mode, "<A-y>", "<Esc>o", { silent = true })
--
-- set(mode, "<a-k>", function()
-- 	if ls.jumpable(1) then
-- 		ls.jump(1)
-- 	end
-- end, { silent = true })
-- set(mode, "<a-j>", function()
-- 	if ls.jumpable(-1) then
-- 		ls.jump(-1)
-- 	end
-- end, { silent = true })
--
-- -- More Settings --
-- - to go to correspoding snippet file
set("n", "<leader>Sn", "<cmd>LuaSnipEdit<cr>", OPTS)
-- reload all snippets
set("n", "<leader>So", "<cmd>source ~/.config/nvim/lua/plugins/luasnip.lua<cr>", OPTS)
-- - enter into custom snippit creation area in the file
vim.cmd([[autocmd BufEnter */snippets/*.lua nnoremap <silent> <buffer> <CR> /-- End Refactoring --<CR>O<Esc>O]]) -- HERE !

vim.api.nvim_create_autocmd("InsertLeave", {
	callback = function()
		local active_node = ls.session.current_nodes[1]
		if not active_node then
			return
		end
		local snippet = active_node.parent.snippet
		snippet:set_ext_opts("snippet_passive")
		snippet.mark:update_opts(snippet.ext_opts.snippet_passive)
	end,
})
