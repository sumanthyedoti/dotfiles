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
local filename = utils.filename
local currentline = utils.currentline
local same = utils.same

local snippets, autosnippets = {}, {}
local cs = utils.create_snippet(snippets, autosnippets)

-- Start Refactoring --
cs("useg", t("use GenServer"))
cs("uses", t("use Supervisor"))
cs("implg", t("@impl GenServer"))
cs("fn", fmt([[ fn {} -> {} end ]], { i(1, "param"), i(2, "body") }))

-- End Refactoring --

return snippets, autosnippets
