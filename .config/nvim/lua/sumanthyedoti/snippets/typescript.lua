local utils = require("sumanthyedoti.snippets.utils")

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
--[[ Dynamic nodes
-- simlar to function nodes, but instead of returning strings, it can return
-- snippet node
--]]
--[[ s vs sn
-- s take trigger
-- sn does not take trigger,
--]]
local snippets, autosnippets = {}, {}

local create_snippet = function(table)
  return function(trigger, nodes, opts)
    local snippet = s(trigger, nodes, opts)
    table.insert(table, snippet)
  end
end

local cs = utils.create_snippet(snippets, autosnippets)

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
cs("cll", { t("console.log("), i(1, ""), t(")") }, { "<leader>cl" }, true)
cs("cw", { t("console.warn("), i(1, ""), t(")") }, { "<leader>ce" })
cs("cww", { t("console.warn("), i(1, ""), t(")") }, { "<leader>ce" }, true)
cs("ce", { t("console.error("), i(1, ""), t(")") }, { "<leader>ce" })
cs("cee", { t("console.error("), i(1, ""), t(")") }, { "<leader>ce" }, true)

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

-- ## class
cs(
  "constructor",
  fmt(
    [[
    constructor({}) {{{}}}
    ]],
    {
      c(1, {
        i(0, ""),
        fmt(
          [[
        {} {}: {}
        ]],
          {
            c(1, {
              t("private"),
              t("public"),
            }),
            i(2, "name"),
            i(3, "Type"),
          }
        ),
      }),
      i(2, ""),
    }
  )
)

--[[ ==== Nestjs ==== ]]
cs(
  "postdto",
  fmt(
    [[
  @Post()
  {}(@Body({}) {}: {}) {{}}
]],
    {
      i(1, "handlerName"),
      i(2, ""),
      i(3, "dto"),
      i(4, "Dto"),
    }
  )
)

cs(
  "poststr",
  fmt(
    [[
  @Post()
  {}(@Body('{}') {}: {}) {{}}
]],
    {
      i(1, "handlerName"),
      i(2, "id"),
      rep(2),
      i(3, "string"),
    }
  )
)

cs(
  "postnum",
  fmt(
    [[
  @Post()
  {}(@Body('{}', ParseIntPipe) {}: {}) {{}}
]],
    {
      i(1, "handlerName"),
      i(2, "id"),
      rep(2),
      i(3, "number"),
    }
  )
)

return snippets, autosnippets
