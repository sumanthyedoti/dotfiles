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

cs("stringlength", fmt([[ {}.chars().count() ]], { i(1, "str") }))

cs(
	"readline",
	fmt(
		[[
    let mut {} = String::new();
    io::stdin()
        .read_line(&mut {})
        .expect("Failed to read line");
]],
		{ i(1, "str"), same(1) }
	)
)

cs(
	"flush",
	fmt(
		[[
use std::io;
use std::io::Write;

fn flush() {{
    io::stdout().flush().unwrap();
}}
]],
		{}
	)
)

-- End Refactoring --

return snippets, autosnippets
