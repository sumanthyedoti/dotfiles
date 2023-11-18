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
cs("tag", fmt([[<=% {} %>]], { i(1, "") }))

-- End Refactoring --

return snippets, autosnippets
