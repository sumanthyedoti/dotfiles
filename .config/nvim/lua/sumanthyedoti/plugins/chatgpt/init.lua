-- return {}
return {
  "jackMort/ChatGPT.nvim",
  event = "VeryLazy",
  dependencies = {
    "MunifTanjim/nui.nvim",
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
  },
  keys = {
    { "<leader>pc", ":ChatGPT<cr>", mode = { "n", "v" } },
    { "<leader>pC", ":ChatGPT<cr>:ChatGPT<cr><C-w><C-w>", mode = { "n", "v" } },
    { "<leader>p", ":ChatGPT<cr>:ChatGPT<cr>", mode = { "n", "v" } },
    { "<leader>pa", ":ChatGPTActAs<cr>", mode = { "n", "v" } },
    { "<leader>ac", ":ChatGPTCompleteCode<cr>", mode = { "n", "v" } },
    { "<leader>ai", ":ChatGPTEditWithInstructions<cr>", mode = { "n", "v" } },
  },
  config = function()
    local chatgpt = require("chatgpt")
    chatgpt.setup({
      edit_with_instructions = {
        diff = false,
        keymaps = {
          close = "<C-c>",
          accept = "<C-y>",
          toggle_diff = "<C-d>",
          toggle_settings = "<C-o>",
          cycle_windows = "<Tab>",
          use_output_as_input = "<C-i>",
        },
      },
      chat = {
        keymaps = {
          close = { "<C-c>" },
          yank_last = "<C-y>", -- <--
          yank_last_code = "<C-k>", -- <--
          scroll_up = "<C-u>",
          scroll_down = "<C-d>",
          new_session = "<C-n>",
          cycle_windows = "<Tab>",
          cycle_modes = "<C-f>",
          select_session = "<C-p>",
          rename_session = "r",
          delete_session = "d",
          draft_message = "<C-t>",
          toggle_settings = "<C-o>",
          toggle_message_role = "<C-r>",
          toggle_system_role_open = "<C-s>",
        },
      },
      popup_layout = {
        default = "center",
        center = {
          width = "96%",
          height = "90%",
        },
        right = {
          width = "40%",
          width_settings_open = "50%",
        },
      },
      openai_params = {
        model = "gpt-3.5-turbo",
        frequency_penalty = 0,
        presence_penalty = 0,
        max_tokens = 600,
        temperature = 0,
        top_p = 1,
        n = 1,
      },
      openai_edit_params = {
        model = "code-davinci-edit-001",
        temperature = 0,
        top_p = 1,
        n = 1,
      },
      api_key_cmd = "pass show api-tokens/openai/sumanthy14",
      predefined_chat_gpt_prompts = "https://raw.githubusercontent.com/sumanthyedoti/dotfiles/main/.config/nvim/lua/plugins/chatgpt/prompts.csv",
    })
  end,
}
