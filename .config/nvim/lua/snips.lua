local ls = require("luasnip")
local fmt = require("luasnip.extras.fmt").fmt
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local c = ls.choice_node
local f = ls.function_node -- dynamic
local d = ls.dynamic_node

--[[ use `i(0)` for final position after all insertion ]]

ls.add_snippets("all", {
	s(
		"date",
		c(1, {
			f(function()
				return os.date("%d-%m-%Y")
			end),
			f(function()
				return os.date("%Y-%m-%d")
			end),
		})
	),
})

ls.add_snippets("lua", {
	s(
		"var",
		c(1, {
			{
				t("local "),
				i(1, "name"),
				t(" = "),
				i(2, "value"),
			},
			fmt("_G.{} = {}", {
				i(1, "name"),
				i(2, "value"),
			}),
			fmt("{} = {}", {
				i(1, "NAME"),
				i(2, "value"),
			}),
		})
	),
	s(
		"req",
		c(1, {
			fmt('local {} = require("{}")', {
				f(function(values)
					local value = values[1][1]
					local path = vim.split(value, "%.")
					return path[#path]
				end, { 1 }),
				i(1),
			}),
			fmt('local {} = require("{}")', {
				f(function(values)
					local value = values[1][1]
					local path = vim.split(value, "%.")
					return table.concat(path, "_")
				end, { 1 }),
				i(1),
			}),
		})
	),
})
