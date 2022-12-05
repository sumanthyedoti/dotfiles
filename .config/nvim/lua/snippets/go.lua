--[[
==== KEY BINDINGS ====
<C-l> -- cycle throught the choices
<C-o> -- close suggestions PUM
]]

local ls_ok, ls = pcall(require, "luasnip")
if not ls_ok then
	vim.notify("luasnip load failed")
	return
end

require("luasnip/loaders/from_vscode").lazy_load()

local s = ls.s
local i = ls.i
local t = ls.t

local c = ls.choice_node
local f = ls.function_node
local d = ls.dynamic_node
local sn = ls.snippet_node

local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

local snippets, autosnippets = {}, {} --}}}

local filename = function()
	return f(function(_args, snip)
		local name = vim.split(snip.snippet.env.TM_FILENAME, ".", true)
		return name[1] or ""
	end)
end

local same = function(index)
	return f(function(args)
		return args[1]
	end, { index })
end

local group = vim.api.nvim_create_augroup("Js Snippets", { clear = true })
local file_pattern = "*.js"

local function cs(trigger, nodes, opts) --{{{
	local snippet = s(trigger, nodes)
	local target_table = snippets

	local pattern = file_pattern
	local keymaps = {}

	if opts ~= nil then
		-- check for custom pattern
		if opts.pattern then
			pattern = opts.pattern
		end

		-- if opts is a string
		if type(opts) == "string" then
			if opts == "auto" then
				target_table = autosnippets
			else
				table.insert(keymaps, { "i", opts })
			end
		end

		-- if opts is a table
		if opts ~= nil and type(opts) == "table" then
			for _, keymap in ipairs(opts) do
				if type(keymap) == "string" then
					table.insert(keymaps, { "i", keymap })
				else
					table.insert(keymaps, keymap)
				end
			end
		end

		-- set autocmd for each keymap
		if opts ~= "auto" then
			for _, keymap in ipairs(keymaps) do
				vim.api.nvim_create_autocmd("BufEnter", {
					pattern = pattern,
					group = group,
					callback = function()
						vim.keymap.set(keymap[1], keymap[2], function()
							ls.snip_expand(snippet)
						end, { noremap = true, silent = true, buffer = true })
					end,
				})
			end
		end
	end

	table.insert(target_table, snippet) -- insert snippet into appropriate table
end --}}}

-- Start Refactoring --

cs( -- [while] JS While Loop snippet{{{
	"main",
	fmt(
		[[
package main

import (
	"fmt"
)

func main() {{
	fmt.Println("Hello world")
}}
  ]],
		{}
	)
)

-- End Refactoring --

-- ls.filetype_extend("javascript", { "html" })
ls.filetype_extend("typescript", { "javascript" })
ls.filetype_extend("typescriptreact", { "javascript" })
ls.filetype_extend("typescript", { "javascriptreact" })
ls.filetype_extend("typescriptreact", { "javascriptreact" })
-- ls.filetype_extend("javascriptreact", { "html" })
-- ls.filetype_extend("typescriptreact", { "html" })

return snippets, autosnippets
