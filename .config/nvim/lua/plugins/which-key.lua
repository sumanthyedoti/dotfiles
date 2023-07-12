local status_ok, wk = pcall(require, "which-key")
if not status_ok then
	return
end

wk.register({
	s = {
		name = "Slime",
		-- keys are defined in `plugins/init.lua`
	},
}, { prefix = "," })

wk.register({
	c = {
		name = "Conjure",
		-- keys are defined in `plugins/init.lua`
	},
}, { prefix = "," })

wk.register({
	e = {
		name = "Conjure",
	},
}, { prefix = "," })

wk.register({
	a = {
		name = "AI",
	},
}, { prefix = "<leader>" })
