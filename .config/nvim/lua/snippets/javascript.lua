local ls = require("luasnip")
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
local r = utils.r
local filename = utils.filename
local currentline = utils.currentline
local same = utils.same

local snippets, autosnippets = {}, {}
local javascript, react = {}, {}

local create_snippet = function(table)
	return function(trigger, nodes, opts)
		local snippet = s(trigger, nodes, opts)
		table.insert(table, snippet)
	end
end

local cs = utils.create_snippet(snippets)
local cas = utils.create_snippet(snippets)
local cs_js = utils.create_snippet(javascript)
local cs_react = utils.create_snippet(react)

-- Start Refactoring --
cs(
	{ trig = "for(%w+)", regTrig = true, hidden = true },
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
)

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
cs("f_randInRange", {
	t({ "function randomNumberInRange(min, max) {" }),
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
cs("f_randomizeArray", {
	t({ "function randomizeArray(arr) {" }),
	t({ "  return arr.sort(() => 0.5 - Math.random())", "" }),
	t("}"),
})

-- Testting
cs(
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

-- React
cs("imr", t('import React from "react"'))
cs("improptypes", t('import PropTypes from "prop-types"'))
cs(
	"proptypes",
	fmt(
		[[
{}.propTypes = {{
  {}: PropTypes.string
}}
  ]],
		{
			filename(),
			i(1, "title"),
		}
	)
)
cs("imrn", fmt('import {{ {} }} from "react-native"', i(1, "View")))

-- React Native
cs(
	"stylesrn",
	fmt(
		[[
const styles = StyleSheet.create({{
  {}: {{
    {}: {}
  }}
}})
  ]],
		{
			i(1, "container"),
			i(2, "flex"),
			i(3, "1"),
		}
	)
) --}}}
cs(
	"rnbutton",
	fmt(
		[[
      <Button onPress={} title="{}" />
]],
		{
			i(0, "onPress"),
			i(0, "Press Here"),
		}
	)
)

cs("rnsfcc", {
	t({ "alignItems: 'center',", "" }),
	t({ "justifyContent: 'center',", "" }),
})
cs("rnstest", {
	t({ "borderColor: '#f00',", "" }),
	t({ "borderStyle: 'solid',", "" }),
	t({ "borderWidth: 1,", "" }),
})

cs("rndim", {
	t({ "const {width: SCREEN_WIDTH, height: SCREEN_HEIGHT} = Dimensions.get('window')", "" }),
})
cs("rnusedim", {
	t({ "const { width: SCREEN_WIDTH, height: SCREEN_HEIGHT } = useWindowDimensions()", "" }),
})

-- RN reanimated

cs("imrean", {
	t({ "import Animated from 'react-native-reanimated'", "" }),
})

cs(
	"reanscroll",
	fmt(
		[[
      const {} = useSharedValue(0)

      const {} = useAnimatedScrollHandler({{
        onScroll: (event) => {{
          {}.value = event.contentOffset.y
        }},
      }})
    ]],
		{
			i(1, "scrollOffsetY"),
			i(2, "scrollHandler"),
			rep(1),
		}
	)
)

cs( -- useSharedValue
	"reanusesh",
	fmt(
		[[
      const {} = useSharedValue({})
    ]],
		{
			i(1, "sharedValue"),
			i(2, "0"),
		}
	)
)

cs( -- useAnimatedStyle
	"reanstyle",
	fmt(
		[[
  const {} = useAnimatedStyle(() => {{
    return {{
    {}
    }}
  }})
    ]],
		{
			i(1, "rStyles"),
			i(2, ""),
		}
	)
)

cs( -- useAnimatedStyle with translation
	"reanstyletanslate",
	fmt(
		[[
  const {} = useAnimatedStyle(() => {{
    return {{
      transform: [
        {{
          translateX: {}.value,
        }},
        {{
          translateY: {}.value,
        }},
      ],
    }}
  }})
    ]],
		{
			i(1, "rStyles"),
			i(2, "touchX"),
			i(3, "touchY"),
		}
	)
)

-- End Refactoring --

ls.filetype_extend("javascript", { "javascriptreact", "typescript", "typescriptreact" })

return snippets, autosnippets
