local ls_ok, ls = pcall(require, "luasnip")
if not ls_ok then
	vim.notify("luasnip load failed")
	return
end

-- = Expansions =
-- TM_CURRENT_LINE -- line number
-- TM_FILENAME -- file name
-- TM_DIRECTORY -- parent directory name

local M = {}

M.s = ls.snippet -- takes trigger
M.t = ls.text_node
M.i = ls.insert_node
M.c = ls.choice_node --
M.r = ls.restore_node
M.f = ls.function_node -- can only return string, can take other node positons as dependecies
M.d = ls.dynamic_node -- like function node but can take positon number (insert editable) and can return other nodes with snippet_node
M.sn = ls.snippet_node -- does not take trigger, groups other nodes

M.fmt = require("luasnip.extras.fmt").fmt
M.rep = require("luasnip.extras").rep

M.filename = function()
	return M.f(function(_args, snip)
		local name = vim.split(snip.snippet.env.TM_FILENAME, ".", true)
		return name[1] or ""
	end)
end

M.currentline = function()
	return M.f(function(_args, snip)
		local name = vim.split(snip.snippet.env.TM_CURRENT_LINE, ".", true)
		return name[1] or ""
	end)
end

M.same = function(index)
	return M.f(function(args)
		return args[1]
	end, { index })
end

M.create_snippet = function(snippets, autosnippets)
	local cs = function(trigger, nodes, opts, is_auto_snip)
		local snippet = M.s(trigger, nodes, opts)
		local target_table = snippets
		if is_auto_snip then
			table.insert(autosnippets, snippet)
		else
			table.insert(target_table, snippet)
		end
	end
	return cs
end

return M
