return {
  "nvim-orgmode/orgmode",
  event = "VeryLazy",
  dependencies = {
    { "nvim-treesitter/nvim-treesitter", lazy = true },
  },
  config = function()
    local orgmode = require("orgmode")
    -- Load treesitter grammar for org
    orgmode.setup_ts_grammar()

    -- Setup treesitter
    require("nvim-treesitter.configs").setup({
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = { "org" },
      },
      ensure_installed = { "org" },
    })

    -- Setup orgmode
    orgmode.setup({
      -- org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
      org_agenda_files = "~/org/**/*",
      org_default_notes_file = "~/org/NOTES.org",
    })
  end,
}
