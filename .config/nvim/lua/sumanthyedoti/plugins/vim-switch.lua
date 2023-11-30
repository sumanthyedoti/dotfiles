return {
  "AndrewRadev/switch.vim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    vim.api.nvim_set_keymap("n", "<leader>`", ":Switch<CR>", OPTS)
    vim.api.nvim_set_keymap("n", "<leader>~", ":SwitchReverse<CR>", OPTS)
    vim.g.switch_mapping = "" -- avoid defalt mapping
    vim.g.switch_custom_definitions = {
      { "_", "-" },
      { "null", "undefined" },
      { "true", "false" },
      { "True", "False" },
      { "const", "let" },
      { "int", "float" },
      { "||", "&&" },
      { "and", "or" },
      { "> ", "<= " },
      { ">= ", "< " },
      { "def", "defn" },
      { "absolute", "relative" },
      { "width", "height" },
      { "horizontal", "vertical" },
      { "padding", "margin" },
      { "number", "string" },
      { "log", "warn", "error" },
      { "top", "bottom" },
      { "left", "right" },
      { "public", "private", "protected" },
      { "foo", "bar", "baz" },
      -- english opposite words
      { "always", "never" },
      { "Always", "Never" },
      { "slow", "fast" },
      { "Slow", "Fast" },
      { "good", "bad" },
      { "Good", "Bad" },
      { "correct", "wrong" },
      { "Correct", "Wrong" },
      { "big", "small" },
      { "Big", "Small" },
      { "bigger", "smaller" },
      { "Bigger", "Smaller" },
      { "biggest", "smallest" },
      { "Biggest", "Smallest" },
      { "high", "low" },
      { "High", "Low" },
      { "inside", "outside" },
      { "Inside", "Outside" },
      { "weak", "strong" },
      { "Weak", "Strong" },
      { "open", "close" },
      { "Open", "Close" },
      { "opened", "closed" },
      { "start", "end" },
      { "Start", "End" },
      { "starting", "ending" },
      { "Starting", "Ending" },
      { "front", "back" },
      { "Front", "Back" },
      { "up", "down" },
      { "new", "old" },
      { "New", "Old" },
      { "light", "dark" },
      { "Light", "Dark" },
    }

    vim.cmd([[
    let b:switch_custom_definitions = [
    \   {
    \     '\<[a-z0-9]\+_\k\+\>': {
    \       '_\(.\)': '\U\1'
    \     },
    \     '\<[a-z0-9]\+[A-Z]\k\+\>': {
    \       '\([A-Z]\)': '_\l\1'
    \     },
    \   }
    \ ]
    ]])
  end,
}
