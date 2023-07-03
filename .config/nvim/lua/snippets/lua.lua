local utils = require("snippets.utils")
local s = utils.s
local f = utils.f
local d = utils.d
local fmt = utils.fmt
local rep = utils.rep
local i = utils.i
local t = utils.t
local sn = utils.sn
local c = utils.c
local filename = utils.filename
local currentline = utils.currentline
local same = utils.same

local snippets, autosnippets = {}, {}
local cs = utils.create_snippet(snippets, autosnippets)

-- Start Refactoring --
cs("CMD", { -- [CMD] multiline vim.cmd{{{
	t({ "vim.cmd[[", "  " }),
	i(1, ""),
	t({ "", "]]" }),
}) --}}}
cs("cmd", fmt("vim.cmd[[{}]]", { i(1, "") })) -- single line vim.cmd

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
	sn(1, { t('t({"'), i(1, "line"), t('", ""})') }),
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
	fmt("math.random(1, {})", {
		i(1, "100"),
	})
)

-- End Refactoring --

return snippets, autosnippets
