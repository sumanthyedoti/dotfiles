local utils = require("snippets.utils")
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

cs(
	"#s",
	fmt(
		[[
#!/usr/bin/python


  ]],
		{}
	),
	{},
	true
)

cs("get_cmd_output", {
	t({ "def get_cmd_output(cmd: str):", "" }),
	t({ "    result = subprocess.check_output(cmd, shell=True)", "" }),
	t({ "    return result.decode().strip()", "" }),
})

-- End Refactoring --

return snippets, autosnippets
