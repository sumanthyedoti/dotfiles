-- submodes and menus
return {
  "anuvyklack/hydra.nvim",
  event = "BufEnter",
  config = function()
    local status_ok, Hydra = pcall(require, "hydra")

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
      body = "<leader>W",
      config = {
        -- color = "pink",
      },
      heads = {
        -- move between windows
        { "<left>", "<C-w>1>" },
        { "<right>", "<C-w>1<" },
        { "<up>", "<C-w>1+" },
        { "<down>", "<C-w>1-" },

        -- split
        { "s", ":split<CR>" },
        { "v", ":vsplit<CR>" },

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

  end,
} 
