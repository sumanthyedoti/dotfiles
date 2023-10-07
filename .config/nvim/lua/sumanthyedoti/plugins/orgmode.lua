return {
  "nvim-orgmode/orgmode",
  config = function()
    local orgmode = require("orgmode")
    if not status_ok then
      return
    end

    orgmode.setup_ts_grammar()

    orgmode.setup {
      -- org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
      -- org_default_notes_file = '~/Dropbox/org/refile.org',
    }

  end,
}
