-- timer
local function count_with_timer()
	local timer = vim.loop.new_timer()
	local i = 0
	timer:start(0, 500, function()
		print("count: " .. tostring(i))
		if i > 10 then
			timer:close()
			print("timer ended")
		end
		i = i + 1
	end)
end
-- count_with_timer()

local function delayed_call(cb)
	vim.defer_fn(cb, 2000)
end
local print_after_delay = function()
	print("Time is up!")
end
-- delayed_call(print_after_delay)

local function vim_api_in_loop_callback()
	local timer = vim.loop.new_timer()
	local i = 0
	timer:start(
		0,
		500,
		-- vim.api calls in loop callback errors without schedule_wrap
		vim.schedule_wrap(function()
			-- prints at the EOF
			vim.api.nvim_buf_set_lines(0, -1, -1, true, { "count: " .. tostring(i) })
			if i > 10 then
				timer:close()
				print("timer ended")
			end
			i = i + 1
		end)
	)
end
-- vim_api_in_loop_callback()

--== Neovim embeds the libuv library in the editor and exposes lua bindings for interacting with the library’s API
local function shell_command()
	local libuv = vim.loop
	local cmd = "touch"
	local options = {
		args = { "1.txt", "2.txt" },
	}
	local handle
	local on_exit = function(status)
		libuv.close(handle)
		print("exited...", status)
	end
	handle = libuv.spawn(cmd, options, on_exit)
end
-- shell_command()

local function read_from_shell()
	local libuv = vim.loop
	local cmd = "ls"
	local output_pipe = libuv.new_pipe()
	local options = {
		args = {},
		stdio = { nil, output_pipe, nil }, -- {stdin, stdout, stderr}
	}
	local handle
	local on_exit = function(status)
		libuv.read_stop(output_pipe)
		libuv.close(output_pipe)
		libuv.close(handle)
		print("exited...", status)
	end
	handle = libuv.spawn(cmd, options, on_exit)
	libuv.read_start(output_pipe, function(status, data)
		if data then
			print(data) -- see the output in :messages
		end
	end)
end
-- read_from_shell()

local function feedkeys(key, mode)
	vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
end
-- inserts text at top of the file without moving the cursor
vim.keymap.set("n", "zf", function()
	feedkeys("gg", "n")
	feedkeys("^", "n")
	feedkeys("O#Hello", "n")
	feedkeys("<esc>", "n")
	feedkeys("<C-o>", "n")
end)
