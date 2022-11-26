--[[
==== KEY BINDINGS ====
<C-l> -- cycle throught the choices
<C-o> -- close suggestions PUM
]]

local ls = require("luasnip") --{{{
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

cs( -- for([%w_]+) JS For Loop snippet{{{
	{ trig = "for([%w_]+)", regTrig = true, hidden = true },
	fmt(
		[[
      for (let {} = 0; {} < {}; {}++) {{
        {}
      }}
      {}
    ]],
		{
			d(1, function(_, snip)
				return sn(1, i(1, snip.captures[1]))
			end),
			rep(1),
			c(2, { i(1, "num"), sn(1, { i(1, "arr"), t(".length") }) }),
			rep(1),
			i(3, "// TODO:"),
			i(4),
		}
	)
) --}}}

cs( -- [while] JS While Loop snippet{{{
	"while",
	fmt(
		[[
while ({}) {{
  {}
}}
  ]],
		{
			i(1, ""),
			i(2, "// TODO:"),
		}
	)
) --}}}
cs("cl", { t("console.log("), i(1, ""), t(")") }, { "<leader>cl" })
cs("ce", { t("console.error("), i(1, ""), t(")") }, { "<leader>ce" })
-- ## Util Function
cs("f_isEven", t("const isEven = num => num % 2 === 0"))
cs("f_isOdd", t("const isOdd = num => num % 2 !== 0"))
cs("f_isNumber", {
	t({ "function isNumber(num) {", "" }),
	t({ "  return !isNaN(parseFloat(num)) && isFinite(num)", "" }),
	t("}"),
})
cs("f_isObjectEmpty", {
	t({ "const isObjectEmpty = (object) => {", "" }),
	t({ "  if (object.constructor !== Object) return false;", "" }),
	t({ "  // if key exists, return false", "" }),
	t({ "  for (_ in object) return false", "" }),
	t({ "  return true;", "" }),
	t("}"),
})
cs("function f_randomNumberInRange(min, max) {", {
	t({ "  min = Math.ceil(min)", "" }),
	t({ "  max = Math.floor(max)", "" }),
	t({ "  return Math.floor(Math.random() * (max - min + 1)) + min", "" }),
	t("}"),
})
cs("f_uniqueValues", {
	t({ "const uniqueValues = (array) => {", "" }),
	t({ "  const uniqueValues = []", "" }),
	t({ "  const seenMap = {}", "" }),
	t({ "  for (const item of array) {", "" }),
	t({ "    if (seenMap[item]) continue", "" }),
	t({ "    seenMap[item] = true", "" }),
	t({ "    uniqueValues.push(item)", "" }),
	t({ "  }", "" }),
	t({ "  return uniqueValues;", "" }),
	t("}"),
})
cs("f_client_scrollToTop", t("const scrollToTop = () => window.scrollTo(0, 0)"))

cs( -- [while] JS While Loop snippet{{{
	"test",
	fmt(
		[[
test ("{}") () => {{
  {}
}})
  ]],
		{
			i(1, "test_message"),
			i(2, "// test logic"),
		}
	)
)
cs(
	"stylesrn",
	fmt(
		[[
const styles = StyleSheet.create({{
  {}: {{
    {}
  }}
}})
  ]],
		{
			i(1, "container"),
			i(2, ""),
		}
	)
) --}}}

-- End Refactoring --

return snippets, autosnippets
