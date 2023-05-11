local status_ok, orgmode = pcall(require, "orgmode")
if not status_ok then
	return
end

orgmode.setup_ts_grammar()

require("orgmode").setup({
	org_agenda_files = { "~/org/**/*" },
	org_default_notes_file = "~/org/refile.org",
})

vim.opt.conceallevel = 2
vim.opt.concealcursor = "nc"
vim.opt.shellslash = true
