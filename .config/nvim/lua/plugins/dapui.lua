local status_ok, dap = pcall(require, "dap")
if not status_ok then
	return
end

local ui_status_ok, dapui = pcall(require, "dapui")
if not ui_status_ok then
	return
end

dapui.setup()

dap.listeners.after.event_initialized["dapui_config"] = function()
	print("sdfsdf")
	dapui.open()
end

dap.listeners.before.event_terminated["dapui_config"] = function()
	dapui.close()
end

dap.listeners.before.event_exited["dapui_config"] = function()
	dapui.close()
end
