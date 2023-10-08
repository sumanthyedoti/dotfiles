local generate_co_authors = function()
  local output = vim.api.nvim_call_function("system", { 'git log --all --format="%cN <%cE>" | sort -u' })
  local co_authors = vim.split(output, "\n")
  return co_authors
end

local list_co_authors = function()
  local co_authors = generate_co_authors()
  if #co_authors > 0 then
    vim.ui.select(co_authors, { prompt = "Select Co-Authors" }, function(item, _)
      if not item then
        return
      end
      local string = "Co-authored-by: " .. item
      local cursor_position = vim.api.nvim_win_get_cursor(0)
      local line = cursor_position[1]
      local column = cursor_position[2]

      vim.api.nvim_buf_set_lines(0, line, line, true, { string })
      vim.api.nvim_win_set_cursor(0, { line, column + #string })
    end)
  end
end

vim.keymap.set("n", "<leader>gC", list_co_authors)
