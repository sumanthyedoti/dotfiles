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
local filename = utils.filename
local currentline = utils.currentline
local same = utils.same

local snippets, autosnippets = {}, {}
local cs = utils.create_snippet(snippets, autosnippets)

-- Start Refactoring --
cs(
  "date",
  c(1, {
    f(function()
      return os.date("%d-%m-%Y")
    end),
    f(function()
      return os.date("%Y-%m-%d")
    end),
  })
)

cs("hw", t("Hello, World!"))

cs(
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
)
cs(
  "lorems",
  t(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
  )
)

-- End Refactoring --

return snippets, autosnippets
