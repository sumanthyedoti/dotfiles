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
	s(
		"loremp",
		c(1, {
			t(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer tempus, ipsum in pulvinar placerat, tellus leo pharetra tortor, nec sollicitudin lectus odio nec sapien. Duis felis dui, molestie nec convallis sed, ornare lacinia lectus. Proin bibendum nisl quis arcu accumsan, ac vehicula leo luctus. Cras eget nunc et erat lobortis egestas. Vestibulum accumsan augue ligula, ut dignissim mauris auctor ac. Sed porta ipsum bibendum auctor mattis. Morbi at semper dolor."
			),
			t(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer tempus, ipsum in pulvinar placerat, tellus leo pharetra tortor, nec sollicitudin lectus odio nec sapien. Duis felis dui, molestie nec convallis sed, ornare lacinia lectus. Proin bibendum nisl quis arcu accumsan, ac vehicula leo luctus. Cras eget nunc et erat lobortis egestas. Vestibulum accumsan augue ligula, ut dignissim mauris auctor ac. Sed porta ipsum bibendum auctor mattis. Morbi at semper dolor. Nullam non elit at turpis facilisis faucibus. Donec vestibulum elementum tellus, nec aliquet eros sagittis et. Proin suscipit odio in mauris malesuada, ut pretium libero varius. Suspendisse potenti."
			),
			t(
				"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Egestas egestas fringilla phasellus faucibus scelerisque eleifend donec pretium vulputate. Elementum tempus egestas sed sed risus. Suscipit adipiscing bibendum est ultricies integer quis auctor elit sed. Enim neque volutpat ac tincidunt vitae semper quis. Pellentesque pulvinar pellentesque habitant morbi tristique. Varius sit amet mattis vulputate enim. Fermentum posuere urna nec tincidunt praesent semper feugiat. Sit amet cursus sit amet dictum. Pellentesque diam volutpat commodo sed egestas egestas. Etiam tempor orci eu lobortis elementum nibh tellus. Pharetra convallis posuere morbi leo urna molestie at elementum eu. Arcu cursus euismod quis viverra nibh. Lectus magna fringilla urna porttitor rhoncus dolor purus. Eros in cursus turpis massa tincidunt dui ut ornare lectus. Neque egestas congue quisque egestas diam."
			),
		})
	),
	s(
		"lorems",
		t(
			"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
		)
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
