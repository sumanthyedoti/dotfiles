return {
  "ThePrimeagen/harpoon",
  config = function()
    local mark = require("harpoon.mark")
    local ui = require("harpoon.ui")
    local function map(mode, l, r, opts)
      opts = opts or {}
      vim.keymap.set(mode, l, r, opts)
    end

    map("n", "<leader>ma", mark.add_file, OPTS)
    map("n", "<leader>mm", ui.nav_next, OPTS)
    map("n", "<leader>mn", ui.nav_prev, OPTS)
    map("n", "<leader>m ", ui.toggle_quick_menu, OPTS)
    map("n", "<leader>ms", ":Telescope harpoon marks<cr>", OPTS)
  end,
}
