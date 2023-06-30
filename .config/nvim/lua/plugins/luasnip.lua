local ls_ok, ls = pcall(require, "luasnip")
if not ls_ok then
	vim.notify("luasnip load failed")
	return
end

local M = {}

function M.expand_or_jump()
	if ls.expand_or_jumpable() then
		ls.expand_or_jump()
	end
end

function M.jump_next()
	if ls.jumpable(1) then
		ls.jump(1)
	end
end

function M.jump_prev()
	if ls.jumpable(-1) then
		ls.jump(-1)
	end
end

function M.change_choice()
	if ls.choice_active() then
		ls.change_choice(1)
	end
end

function M.reload_package(package_name)
	for module_name, _ in pairs(package.loaded) do
		if string.find(module_name, "^" .. package_name) then
			package.loaded[module_name] = nil
			require(module_name)
		end
	end
end

function M.refresh_snippets()
	ls.cleanup()
	M.reload_package("snips")
end

local set = vim.keymap.set

local mode = { "i", "s" }
local normal = { "n" }

set(mode, "<c-i>", M.expand_or_jump)
set(mode, "<c-j>", M.jump_prev)
set(mode, "<c-l>", M.change_choice)
set(normal, ",r", M.refresh_snippets)

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
		-- [types.choiceNode] = {
		-- 	active = {
		-- 		virt_text = { { "↢", "GruvboxOrange" } },
		-- 	},
		-- },
		-- [types.insertNode] = {
		-- 	active = {
		-- 		virt_text = { { "●", "GruvboxBlue" } },
		-- 	},
		-- },
	},
}) --}}}
-- Key Mapping --{{{
vim.cmd("imap <silent><expr> <C-l> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-e>'")
vim.cmd("smap <silent><expr> <C-l> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-e>'")
-- vim.keymap.set({ "i", "s" }, "<A-p>", function()
-- 	if ls.expand_or_jumpable() then
-- 		ls.expand()
-- 	end
-- end, { silent = true })
-- -- vim.keymap.set({ "i", "s" }, "<C-k>", function()
-- -- 	if ls.expand_or_jumpable() then
-- -- 		ls.expand_or_jump()
-- -- 	end
-- -- end, { silent = true })
-- -- vim.keymap.set({ "i", "s" }, "<C-j>", function()
-- -- 	if ls.jumpable() then
-- -- 		ls.jump(-1)
-- -- 	end
-- -- end, { silent = true })
--
-- vim.keymap.set({ "i", "s" }, "<A-y>", "<Esc>o", { silent = true })
--
-- vim.keymap.set({ "i", "s" }, "<a-k>", function()
-- 	if ls.jumpable(1) then
-- 		ls.jump(1)
-- 	end
-- end, { silent = true })
-- vim.keymap.set({ "i", "s" }, "<a-j>", function()
-- 	if ls.jumpable(-1) then
-- 		ls.jump(-1)
-- 	end
-- end, { silent = true })
--
-- vim.keymap.set({ "i", "s" }, "<a-l>", function()
-- 	if ls.choice_active() then
-- 		ls.change_choice(1)
-- 	else
-- 		-- print current time
-- 		local t = os.date("*t")
-- 		local time = string.format("%02d:%02d:%02d", t.hour, t.min, t.sec)
-- 		print(time)
-- 	end
-- end)
-- vim.keymap.set({ "i", "s" }, "<a-h>", function()
-- 	if ls.choice_active() then
-- 		ls.change_choice(-1)
-- 	end
-- end) --}}}
--
-- -- More Settings --
-- - to go to correspoding snippet file
vim.keymap.set("n", "<leader>sn<CR>", "<cmd>LuaSnipEdit<cr>", { silent = true, noremap = true }) -- HERE
-- - enter into custom snippit creation area in the file
vim.cmd([[autocmd BufEnter */snippets/*.lua nnoremap <silent> <buffer> <CR> /-- End Refactoring --<CR>O<Esc>O]]) -- HERE !
