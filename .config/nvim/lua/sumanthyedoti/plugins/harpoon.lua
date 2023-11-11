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
    map("n", "<leader>1", ':lua require("harpoon.ui").nav_file(1)<esc>', OPTS)
    map("n", "<leader>2", ':lua require("harpoon.ui").nav_file(2)<esc>', OPTS)
    map("n", "<leader>3", ':lua require("harpoon.ui").nav_file(3)<esc>', OPTS)
    map("n", "<leader>4", ':lua require("harpoon.ui").nav_file(4)<esc>', OPTS)
    map("n", "<leader>5", ':lua require("harpoon.ui").nav_file(5)<esc>', OPTS)
    map("n", "<leader>6", ':lua require("harpoon.ui").nav_file(6)<esc>', OPTS)
    map("n", "<leader>7", ':lua require("harpoon.ui").nav_file(7)<esc>', OPTS)
    map("n", "<leader>8", ':lua require("harpoon.ui").nav_file(8)<esc>', OPTS)
    map("n", "<leader>9", ':lua require("harpoon.ui").nav_file(9)<esc>', OPTS)
  end,
}
