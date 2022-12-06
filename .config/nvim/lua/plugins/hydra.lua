local status_ok, Hydra = pcall(require, "hydra")
if not status_ok then
	return
end

Hydra({
	name = "Side scroll",
	mode = "n",
	body = "z",
	heads = {
		{ "h", "5zh" },
		{ "l", "5zl", { desc = "←/→" } },
		{ "H", "zH" },
		{ "L", "zL", { desc = "half screen ←/→" } },
		-- exit options
		{ "q", nil, { exit = true, nowait = true } },
		{ ";", nil, { exit = true, nowait = true } },
		{ "<Esc>", nil, { exit = true, nowait = true } },
	},
})

-- Hydra({ -- TODO
-- 	name = "LSP",
-- 	mode = "n",
-- 	body = "<leader>l",
-- 	heads = {
-- 		{ "r", "<leader>rn" },
--
-- 		-- exit this Hydra
-- 		{ "q", nil, { exit = true, nowait = true } },
-- 		{ ";", nil, { exit = true, nowait = true } },
-- 		{ "<Esc>", nil, { exit = true, nowait = true } },
-- 	},
-- })

Hydra({
	name = "Change / Resize Window",
	mode = { "n" },
	body = "<C-w>",
	config = {
		-- color = "pink",
	},
	heads = {
		-- move between windows
		{ "h", "<C-w>h" },
		{ "j", "<C-w>j" },
		{ "k", "<C-w>k" },
		{ "l", "<C-w>l" },

		-- resizing window
		{ "H", "<C-w>3<" },
		{ "L", "<C-w>3>" },
		{ "K", "<C-w>2+" },
		{ "J", "<C-w>2-" },
		{ "<C-h>", "<C-w><" },
		{ "<C-l>", "<C-w>>" },
		{ "<C-k>", "<C-w>+" },
		{ "<C-j>", "<C-w>-" },
		-- equalize window sizes
		{ "e", "<C-w>=" },
		-- close active window
		{ "<C-q>", ":q<cr>" },
		-- exit this Hydra
		{ "q", nil, { exit = true, nowait = true } },
		{ ";", nil, { exit = true, nowait = true } },
		{ "<Esc>", nil, { exit = true, nowait = true } },
	},
})
