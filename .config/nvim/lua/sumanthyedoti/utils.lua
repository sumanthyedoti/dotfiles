local M = {}
OPTS = { noremap = true, silent = true } -- { nowait = true }

M.merge_tables = function(t1, t2)
  local all_opts = {}
  for k, v in pairs(t1) do
    all_opts[k] = v
  end
  for k, v in pairs(t2) do
    all_opts[k] = v
  end
  return all_opts
end

M.map_key = function(mode, key, action, extra_opts)
  local all_opts = (extra_opts and M.merge_tables(OPTS, extra_opts)) or OPTS
  local keymap = vim.api.nvim_set_keymap
  keymap(mode, key, action, all_opts)
end

return M
