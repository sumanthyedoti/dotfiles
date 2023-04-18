local status_ok, wilder = pcall(require, "wilder")
if not status_ok then
	return
end

local gradient = {
	"#f4468f",
	"#ff5e63",
	"#ff843d",
	"#f89b31",
	"#e6b32e",
	"#d2c934",
	"#bfde43",
	"#aff05b",
}

for i, fg in ipairs(gradient) do
	gradient[i] = wilder.make_hl("WilderGradient" .. i, "Pmenu", { { a = 1 }, { a = 1 }, { foreground = fg } })
end

-- wilder.set_option(
-- 	"renderer",
-- 	wilder.popupmenu_renderer({
-- 		highlights = {
-- 			gradient = gradient, -- must be set
-- 			-- selected_gradient key can be set to apply gradient highlighting for the selected candidate.
-- 		},
-- 		highlighter = wilder.highlighter_with_gradient({
-- 			wilder.basic_highlighter(), -- or wilder.lua_fzy_highlighter(),
-- 		}),
-- 	})
-- )

wilder.set_option("pipeline", {
	wilder.branch(
		wilder.cmdline_pipeline({
			fuzzy = 1,
			set_pcre2_pattern = 1,
		}),
		wilder.python_search_pipeline({
			pattern = "fuzzy",
		})
	),
})

wilder.set_option(
	"renderer",
	wilder.popupmenu_renderer(wilder.popupmenu_border_theme({
		highlighter = wilder.basic_highlighter(),
		highlights = {
			border = "Normal", -- highlight to use for the border
		},
		-- 'single', 'double', 'rounded' or 'solid'
		-- can also be a list of 8 characters, see :h wilder#popupmenu_border_theme() for more details
		border = "rounded",
		reverse = 1,
	}))
)

wilder.setup({ modes = { ":", "/", "?" } })
