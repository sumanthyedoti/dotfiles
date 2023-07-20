vim.g.firenvim_config = {
	globalSettings = { alt = "all" },
	localSettings = {
		[".*"] = {
			cmdline = "neovim",
			content = "text",
			priority = 0,
			selector = "textarea",
			takeover = "always",
		},
	},
}

vim.cmd([[
 function! s:setup_firenvim() abort
   nnoremap <leader>Fh :set lines=20<CR>
   nnoremap <leader>FH :set lines=40<CR>
   nnoremap <leader>Fw :set columns=50<CR>
   nnoremap <leader>FW :set columns=100<CR>

   nnoremap <C-j> :set &lines - 1<CR>
   nnoremap <C-k> :set lines=40<CR>
   nnoremap <C-h> :set columns=50<CR>
   nnoremap <C-l> :set columns=100<CR>

   set lines=10
   set columns=70
   set filetype=markdown
   set cmdheight=1
   set noruler noshowcmd
   set laststatus=0 showtabline=0
 endfunction

 augroup firenvim
 autocmd!
   autocmd FileType text call s:setup_firenvim()
 augroup END
]])
