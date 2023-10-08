local utils = require("sumanthyedoti.snippets.utils")
local s = utils.s
local f = utils.f
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
cs("defvar", fmt([[ (defvar *{}* {}) ]], { i(1, "x"), i(2, "10") }))

cs("defparameter", fmt([[ (defparameter *{}* {}) ]], { i(1, "x"), i(2, "10") }))

cs(
  "defun",
  fmt(
    [[
  (defun {} ({})
    ({})) ]],
    { i(1, "funame"), i(2, ""), i(3, "") }
  )
)

cs(
  "defmacro",
  fmt(
    [[
  (demacro {} ({})
    ({})) ]],
    { i(1, "mname"), i(2, ""), i(3, "") }
  )
)

-- End Refactoring --

return snippets, autosnippets
