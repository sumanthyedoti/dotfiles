local add_js_doc_comment = require("sumanthyedoti.custom.js.add_js_doc_comment")

local options = {
	add_js_doc = "Add JS Doc",
}

local function custom_options()
	local ui_options = {}
	for _, option in pairs(options) do
		table.insert(ui_options, option)
	end
	vim.ui.select(ui_options, {
		prompt = "Select an options:",
	}, function(choice)
		if choice == options.add_js_doc then
			add_js_doc_comment()
		else
		end
	end)
end

vim.keymap.set("n", "<leader>js", custom_options)
