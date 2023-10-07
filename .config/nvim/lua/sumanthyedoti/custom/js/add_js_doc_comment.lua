local ts_utils = require("nvim-treesitter.ts_utils")
local ts_locals = require("nvim-treesitter.locals")
local ts_indent = require("nvim-treesitter.indent")

local function get_function_node()
	local current_node = ts_utils.get_node_at_cursor()
	local scope = ts_locals.get_scope_tree(current_node, 0) -- 0 for current buffer
	local func_node = nil

	for _, node in ipairs(scope) do
		print(node:type())
		if node:type() == "function_declaration" then
			func_node = node
		end
	end
	return func_node
end

local function get_function_info()
	local func_node = get_function_node()
	if not func_node then
		return
	end
	local query = vim.treesitter.query.parse(
		"javascript",
		[[
      (function_declaration
        name: (identifier) @function_name
        parameters: (formal_parameters) @params)
    ]]
	)
	for _, matches, _ in query:iter_matches(func_node, 0) do
		local func_name = vim.treesitter.get_node_text(matches[1], 0)
		local params_list = {}

		local param_node = matches[2]
		for param in param_node:iter_children() do
			if param:type() == "identifier" then
				table.insert(params_list, vim.treesitter.get_node_text(param, 0))
			end
		end

		return {
			name = func_name,
			params_list = params_list,
			start_line = func_node:start(),
		}
	end
end

local function get_doc_comment(func_info, tab_space)
	local doc_comment = {}
	local function add_line(line)
		table.insert(doc_comment, tab_space .. line)
	end
	add_line("/**")
	add_line(string.format(" * %s <description>", func_info.name))
	add_line(" *")
	for _, param_name in ipairs(func_info.params_list) do
		add_line(string.format(" * @param {type} %s - <description>", param_name))
	end
	add_line(" * @return {type} Returns <description>")
	add_line(" */")
	add_line("")
	return doc_comment
end

local function get_indent_str(line)
	local indent_count = ts_indent.get_indent(line)
	if indent_count == 0 then
		return ""
	end
	local tabstop = vim.o.tabstop
	local ntabs = (indent_count / tabstop)
	local tab_space = ""
	if vim.o.expandtab then
		tab_space = string.rep(" ", tabstop * ntabs)
	else
		tab_space = string.rep("\t", ntabs)
	end
	return tab_space
end

local function add_doc_comment()
	local ft = vim.bo.filetype
	print(ft)
	if ft ~= "javascript" and ft ~= "javascriptreact" then
		return
	end
	local func_info = get_function_info()
	if not func_info then
		return
	end
	local tab_space = get_indent_str(func_info.start_line)
	local doc_comment = get_doc_comment(func_info, tab_space)
	vim.api.nvim_buf_set_text(0, func_info.start_line, 0, func_info.start_line, 0, doc_comment)
end

return add_doc_comment
