local ls = require("luasnip")
local fmt = require("luasnip.extras.fmt").fmt
local rep = require("luasnip.extras").rep -- repeat
local s = ls.snippet
local ps = ls.parser.parse_snippet
local t = ls.text_node
local i = ls.insert_node
local c = ls.choice_node
local f = ls.function_node -- dynamic
local d = ls.dynamic_node
local r = ls.restore_node

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
	s("fn", t("$TM_FILENAME")),
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
					return path[#path] or ""
				end, { 1 }),
				r(1, "module_name"),
			}),
			fmt('local {} = require("{}")', {
				f(function(values)
					local value = values[1][1]
					local path = vim.split(value, "%.")
					return table.concat(path, "_")
				end, { 1 }),
				r(1, "module_name"),
			}),
		}),
		{
			stored = {
				module_name = i(nil, "module_name"),
			},
		}
	),
	s(
		"fun",
		c(1, {
			fmt(
				[[
          function {}()
            {}
          end
        ]],
				{
					r(1, "func_name"),
					i(2),
				}
			),
			fmt(
				[[
          local {} = function()
            {}
          end
        ]],
				{
					r(1, "func_name"), -- here func_name is not placeholder, but identifier
					i(2),
				}
			),
		}),
		{
			stored = {
				func_name = i(nil, "node"),
			},
		}
	),
})
