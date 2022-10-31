local ls = require("luasnip") --{{{ - LOCAL LS = REQUIRE("LUASNIP") --{{{
local s = ls.s --> snippet
local i = ls.i --> insert node
local t = ls.t --> text node

local c = ls.choice_node
local f = ls.function_node
local d = ls.dynamic_node
local sn = ls.snippet_node

local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep

local snippets, autosnippets = {}, {} --}}}

local group = vim.api.nvim_create_augroup("Lua Snippets", { clear = true })
local file_pattern = "*.lua"

-- == create snippet
-- takes trigger, body, opts -> "trig"/or/{pattern = file_pattern, "trig"}
local function cs(trigger, nodes, opts)
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

cs("CMD", { -- [CMD] multiline vim.cmd{{{
	t({ "vim.cmd[[", "  " }),
	i(1, ""),
	t({ "", "]]" }),
}) --}}}
cs("cmd", fmt("vim.cmd[[{}]]", { i(1, "") })) -- single line vim.cmd
cs({ -- github import for packer{{{
	trig = "https://github%.com/([%w-%._]+)/([%w-%._]+)!",
	regTrig = true,
	hidden = true,
}, {
	t([[use "]]),
	f(function(_, snip)
		return snip.captures[1]
	end),
	t("/"),
	f(function(_, snip)
		return snip.captures[2]
	end),
	t({ [["]], "" }),
	i(1, ""),
}, "auto") --}}}

cs( -- {regexSnippet} LuaSnippet{{{
	"regexSnippet",
	fmt(
		[=[
cs( -- {}
{{ trig = "{}", regTrig = true, hidden = true }}, fmt([[
{}
]], {{
  {}
}}))
      ]=],
		{
			i(1, "Description"),
			i(2, ""),
			i(3, ""),
			i(4, ""),
		}
	),
	{ pattern = "*/snippets/*.lua", "<leader>xs" }
) --}}}

cs( -- [luaSnippet] LuaSnippet{{{
	"luaSnippet",
	fmt(
		[=[
cs("{}", fmt( -- {}
[[
{}
]], {{
  {}
  }}){})
    ]=],
		{
			i(1, ""),
			i(2, "Description"),
			i(3, ""),
			i(4, ""),
			c(5, {
				t(""),
				fmt([[, "{}"]], { i(1, "keymap") }),
				fmt([[, {{ pattern = "{}", {} }}]], { i(1, "*/snippets/*.lua"), i(2, "keymap") }),
			}),
		}
	),
	{ pattern = "*/snippets/*.lua", "jcs" }
) --}}}

cs( -- choice_node_snippet luaSnip choice node{{{
	"choice_node_snippet",
	fmt(
		[[
c({}, {{ {} }}),
]],
		{
			i(1, ""),
			i(2, ""),
		}
	),
	{ pattern = "*/snippets/*.lua", "jcn" }
) --}}}

cs( -- choice_node_snippet luaSnip choice node{{{
	"line",
	sn(1, { t('t({"'), i(1, "line"), t('", ""}') }),
	{ pattern = "*/snippets/*.lua", "jcn" }
) --}}}

cs( -- [function] Lua function snippet{{{
	"function",
	fmt(
		[[
function {}({})
  {}
end
]],
		{
			i(1, ""),
			i(2, ""),
			i(3, ""),
		}
	),
	"ifu"
) --}}}

cs( -- [local_function] Lua function snippet{{{
	"local_function",
	fmt(
		[[
local function {}({})
  {}
end
]],
		{
			i(1, ""),
			i(2, ""),
			i(3, ""),
		}
	),
	"ilf"
) --}}}
cs( -- [local] Lua local variable snippet{{{
	"local",
	fmt(
		[[
local {} = {}
  ]],
		{ i(1, ""), i(2, "") }
	),
	"lv"
) --}}}

-- Tutorial Snippets go here --
local myFirstSnippet = s("t_num", {
	t({ "one", "" }),
	i(1, "2"),
	t({ "", "three", "" }),
	i(2, "4"),
})

local secondSnippet = s(
	"t_fun",
	fmt(
		[[
  local {} = function({})
    {} {{ curly braces }}
  end
]],
		{
			i(1, "name"),
			i(2, "arg"),
			i(3, "-- body"),
		}
	)
)

local choiceSnippet = s(
	"t_ch",
	fmt(
		[[
  local {} = function({})
    {} {{ curly braces }}
  end
]],
		{
			i(1, "name"),
			c(2, { t(""), i(1, "arg") }),
			i(3, "arg"),
		}
	)
)

table.insert(snippets, myFirstSnippet)
table.insert(snippets, secondSnippet)
table.insert(snippets, choiceSnippet)

local auto1 = s({ trig = "t_auto" }, t("-- this is trig is shown in PUM if passed to 'snippets'"))
local auto2 = s({ trig = "t_aauto" }, t("-- this is trig is not shown in PUM if passed to 'autosnippets'"))
local autod = s({ trig = "t_auto%d", regTrig = true }, t("-- this is autosnippet"))
table.insert(snippets, auto1)
-- autosnippets appear as the trigger matches
table.insert(autosnippets, auto2)
table.insert(autosnippets, autod)

-- ## Function Nodes
-- should return string
local firstFuncNode = s({ trig = "t_funn" }, {
	f(function()
		return "from fucntion"
	end),
})
local funcNodeWithCapture = s({ trig = "t-d(%d)(%d)", regTrig = true }, {
	f(function(_, snip)
		return "# " .. snip.captures[1] .. " & " .. snip.captures[2]
	end),
})
local funcNodeWithArgs = s({ trig = "t_arg1", regTrig = true }, {
	i(1, "some"),
	i(2, " "),
	f(function(arg, snip)
		return arg[1][1]
	end, 1),
})
-- or use "rep" simply
local funcNodeWithRep = s({ trig = "t_rep1", regTrig = true }, {
	i(1, "some"),
	i(2, " "),
	rep(1),
})

local funcNodeWithArgs2 = s({ trig = "t_arg2", regTrig = true }, {
	i(1, "fname"),
	t(" "),
	f(function(arg, snip)
		return arg[2][1]:upper() .. " - " .. arg[1][1]:upper()
	end, { 1, 2 }),
	t(" "),
	i(2, "lname"),
})

table.insert(autosnippets, firstFuncNode)
table.insert(autosnippets, funcNodeWithCapture)
table.insert(autosnippets, funcNodeWithArgs)
table.insert(autosnippets, funcNodeWithRep)
table.insert(autosnippets, funcNodeWithArgs2)

cs(
	"rand",
	fmt(
		[[
    math.random({}, {})
  ]],
		{
			i(1, "1"),
			i(1, "n"),
		}
	),
	"<leader>rand"
)

-- End Refactoring --
return snippets, autosnippets
