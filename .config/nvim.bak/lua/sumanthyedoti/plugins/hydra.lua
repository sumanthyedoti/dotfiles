-- submodes and menus
return {
  "anuvyklack/hydra.nvim",
  event = { "BufReadPre", "BufNewFile" },
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
        { "<down>", "<C-w>j" },
        { "<up>", "<C-w>k" },
        { "<left>", "<C-w>h" },
        { "<right>", "<C-w>l" },

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

    local hint = [[
     Arrow^^^^^^   Select region with '<C-v>'
     ^ ^ _K_ ^ ^   _f_: surround it with box
     _H_ ^ ^ _L_
     ^ ^ _J_ ^ ^                      _<Esc>_
    ]]

    Hydra({
      name = "Draw Diagram",
      hint = hint,
      config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
          border = "rounded",
        },
        on_enter = function()
          vim.o.virtualedit = "all"
        end,
      },
      mode = "n",
      body = "<leader>V",
      heads = {
        { "H", "<C-v>h:VBox<CR>" },
        { "J", "<C-v>j:VBox<CR>" },
        { "K", "<C-v>k:VBox<CR>" },
        { "L", "<C-v>l:VBox<CR>" },
        { "f", ":VBox<CR>", { mode = "v" } },
        { "<Esc>", nil, { exit = true } },
      },
    })
  end,
}
