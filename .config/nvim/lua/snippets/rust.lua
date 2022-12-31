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

local s = ls.s --> snippet
local i = ls.i --> insert node
local t = ls.t --> text node

local c = ls.choice_node --
local f = ls.function_node -- can only return string
local d = ls.dynamic_node -- takes positon number and can return other nodes
local sn = ls.snippet_node -- does not take trigger, groups other nodes
-- sn(1, { i(1, "arr"), t(".length") })

-- = Expansions =
-- TM_CURRENT_LINE -- line number
-- TM_FILENAME -- file name
-- TM_DIRECTORY -- parent directory name

local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

local snippets, autosnippets = {}, {} --}}}

local filename = function()
	return f(function(_args, snip)
		local name = vim.split(snip.snippet.env.TM_FILENAME, ".", true)
		return name[1] or ""
	end)
end

local currentline = function()
	return f(function(_args, snip)
		local name = vim.split(snip.snippet.env.TM_CURRENT_LINE, ".", true)
		return name[1] or ""
	end)
end

local same = function(index)
	return f(function(args)
		return args[1]
	end, { index })
end

local group = vim.api.nvim_create_augroup("Lua Snippets", { clear = true })
local file_pattern = "*.lua" -- HERE: change this for 'cs' function

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

cs("stringlength", fmt([[ {}.chars().count() ]], { i(1, "str") }))

cs(
	"readline",
	fmt(
		[[
    let mut {} = String::new();
    io::stdin()
        .read_line(&mut {})
        .expect("Failed to read line");
]],
		{ i(1, "str"), same(1) }
	)
)

cs(
	"flush",
	fmt(
		[[
use std::io;
use std::io::Write;

fn flush() {{
    io::stdout().flush().unwrap();
}}
]],
		{}
	)
)

-- End Refactoring --

return snippets, autosnippets
