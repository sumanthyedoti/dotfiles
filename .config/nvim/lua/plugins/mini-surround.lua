return {
  {
    "echasnovski/mini.surround",
    opts = {
      custom_surroundings = {
        ["("] = { output = { left = "(", right = ")" } },
        ["["] = { output = { left = "[", right = "]" } },
        ["{"] = { output = { left = "{", right = "}" } },
        ["<"] = { output = { left = "<", right = ">" } },
      },
      mappings = {
        add = "sa", -- Add surrounding
        delete = "sd", -- Delete surrounding
        find = "sf", -- Find surrounding (to the right)
        find_left = "sF", -- Find surrounding (to the left)
        highlight = "sh", -- Highlight surrounding
        replace = "sc", -- Replace surrounding
        update_n_lines = "sn", -- Update `n_lines`
      },
    },
  },
}
