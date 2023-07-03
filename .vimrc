" Enable filetype plugins filetype plugin on
filetype indent on

syntax on  " syntax highlighting

"map Esc key to 'jj'
imap jj <Esc>
imap <buffer> <C-;> <esc>A;  " add semicolon
let mapleader = " "  " map leader key (\) to Space bar
" Disbale 'ZZ' command to save and quit
nnoremap Z <C-o>:echom "--> :w :q <-- "<CR>
nnoremap ZZ <C-o>:echom "--> :w :q <-- "<CR>
" yank util EOL instead of whole line. Matching the behavious of D, C
nnoremap Y y$
" Keep cursor at center and in place with n and N(search) and J
nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap J mzJ`z
" Date with F3
nmap <F3> i<C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR><Esc>
imap <F3> <C-R>=strftime("%Y-%m-%d %a %I:%M %p")<CR>
" disable arrow in insert mode
inoremap <Up>     <C-o>:echom "--> k <-- "<CR>
inoremap <Down>   <C-o>:echom "--> j <-- "<CR>
inoremap <Right>  <C-o>:echom "--> l <-- "<CR>
inoremap <Left>   <C-o>:echom "--> h <-- "<CR>
" move file up and down with arrows
nnoremap <Up> <C-y>
nnoremap <Down> <C-e>
" Switch between tabs
nnoremap <Right> gt
nnoremap <Left>  gT
" close tabs
nnoremap <C-c> :tabclose<CR>
" move tabs
nnoremap <leader>< :tabmove -1<CR>
nnoremap <leader>> :tabmove +1<CR>

" Insert mode navigation
" for mac, the actual characters are mapped
inoremap ˚ <Up>
inoremap ∆ <Down>
inoremap ˙ <Esc>bi
inoremap ¬ <Esc>eli
" split window navigation
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <C-l> <C-w><C-w>

" Slect block remap
nnoremap <C-q> g CTRL-H
" Sizing window horizontally
nnoremap <C-[> <C-W><
nnoremap <C-]> <C-W>>
nnoremap <leader>. :vertical resize +5<CR>
nnoremap <leader>, :vertical resize -5<CR>
" Sizing window vertically
nnoremap <C-j> :resize +2<CR>
nnoremap <C-k> :resize -2<CR>
" hide search(find) highlight
" Clear search (highlight)
map <leader><space> :noh<CR>
" Toggle spell check.
map <F5> :setlocal spell!<CR>
" tabs
nnoremap <C-n> :tabnew<Space>
" open File Explorer
nnoremap <leader>ex :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
" closing brace {}
"inoremap {} {<CR>}<Esc>ko
"inoremap { {   }<Escn>hhi
"inoremap {{   {{   }}<Esc>hhhi
"inoremap ( ()<Esc>i

" Make windows to be basically the same size
nnoremap <leader>= <C-w>=
" Moving lines up or down
nnoremap <C-j> mm:m .+1<CR>`m
nnoremap <C-k> mm:m .-2<CR>`m
inoremap <C-j> <esc>mm:m .+1<CR>`ma
inoremap <C-k> <esc>mm:m .-2<CR>`ma
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv
" split line into new line
nnoremap <leader>nl i<CR><ESC>

function! BreakHere()
    s/^\(\s*\)\(.\{-}\)\(\s*\)\(\%#\)\(\s*\)\(.*\)/\1\2\r\1\4\6
    call histdel("/", -1)
endfunction
nnoremap <key> :<C-u>call BreakHere()<CR>

" tabs
set tabstop=2 softtabstop=2
set shiftwidth=2
set expandtab
set smarttab
set smartindent
set autoindent
set number  " line number
set nocompatible   "no compatibility to Vi
set hidden  " Allow hidden buffers. Buffer becomes hidden when it is abandonedj
set ruler " Show file stats
set relativenumber  " reletive line numbers
set nrformats+=alpha  " increment and decrement alphabets also
set nowrap "no wrap when line exceeds screen
set smartcase "case-sensitive search if search patter contains uppercase character
" Persist changes in undodir. Can access the changes even after reboot
set noswapfile
set nobackup
set autoread
set undofile
set undodir=~/.vim/undodir
set wildmenu  " command-line completion operates in an enhanced mode
set textwidth=80
set colorcolumn=+1
set hlsearch  " Highlight search results
set incsearch " incremental search
set ignorecase
set showmatch  " Show matching brackets when text indicator is over them
set mat=2  " How many tenths of a second to blink when matching brackets
set scrolloff=4  " scroll offset
set cmdheight=1
set signcolumn=yes
set completeopt=menuone,noinsert,noselect
set visualbell
set list
set listchars=tab::\ ,eol:¬,extends:>,precedes:<
set encoding=UTF-8
set guifont="JetBrainsMono Nerd Font"

" sync with system clipboard
set clipboard=unnamed
set clipboard=unnamedplus
" map delete without copying to <leader>dd
nnoremap <leader>dd "_dd
" delete selected text without copying and paste
vnoremap <leader>p "_dP
"set foldcolumn=1  " Add a bit extra margin to the left
"set spell
"set spelllang=en_us
"set exrc  " execute project specific .vimrc
" set guicursor=i:blinkwait700-blinkon500-blinkoff150  " use block cursor
set magic  " For regular expressions turn magic on

" To derive project root
if executable('rg')
    let g:rg_derive_root='true'
endif
" Enable 256 colors palette in Gnome Terminal
if $COLORTERM == 'gnome-terminal'
    set t_Co=256
endif
function! ToggleRelativeNumber()
    if &relativenumber
        set norelativenumber
        echo "Reletive numbers turned off."
    else
        set relativenumber
        echo "Relative numbers turned on."
    endif
endfunction
nnoremap <silent> <leader>ren :call ToggleRelativeNumber()<CR>

" Press * to search for the term under the cursor or find a phrase and
" then press a key below to replace all instances of it in the current file.
nnoremap <Leader>r :%s///g<Left><Left>
nnoremap <Leader>rc :%s///gc<Left><Left><Left>

fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun

function SetCursorLine()
  set cursorline  " highlight current line with underline
  hi CursorLine term=bold cterm=bold guibg=Grey18
endfunction
function SetNoCursorLine()
  set nocursorline  " highlight current line with underline
endfunction
augroup SUMANTH_YEDOTI
    autocmd!
    autocmd BufWritePre * :call TrimWhitespace()
    autocmd BufWinEnter * let w:m2=matchadd('ErrorMsg', '%>80v.\+', -1)
    " auto save
    "autocmd TextChanged,TextChangedI <buffer> silent write
    autocmd InsertEnter * call SetCursorLine()
    autocmd InsertLeave * call SetNoCursorLine()
    " autocmd BufNewFile,BufRead *.html.heex  set syntax=html
augroup END
" Only highlight current window
" augroup CursorLine
"   au!
"   au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
"   au WinLeave * setlocal nocursorline
" augroup END

let g:netrw_banner = 0  " no help information at top for netrw
let loaded_netrwelugin = 1  " disable netrw
call plug#begin('~/.vim/plugged')
" Make sure you use single quotes
Plug 'tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-fugitive'
Plug 'APZelos/blamer.nvim'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
" Plug 'tpope/vim-obsession'
Plug 'tmhedberg/matchit'
Plug 'mhinz/vim-signify'
Plug 'mbbill/undotree'
Plug 'alx741/vim-hindent'
Plug 'vim-airline/vim-airline'
Plug 'ryanoasis/vim-devicons'
Plug 'chemzqm/vim-jsx-improve'
Plug 'mattn/emmet-vim'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'neovimhaskell/haskell-vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'mhinz/vim-grepper'
Plug 'preservim/nerdcommenter'
Plug 'machakann/vim-highlightedyank'
Plug 'vim-test/vim-test'
Plug 'tpope/vim-eunuch'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'rust-lang/rust.vim'
Plug 'rescript-lang/vim-rescript'
Plug 'elixir-editors/vim-elixir'

" Themes
Plug 'flazz/vim-colorschemes'
Plug 'arcticicestudio/nord-vim'
Plug 'overcache/NeoSolarized'
Plug 'morhetz/gruvbox'
Plug 'rakr/vim-one'
call plug#end()

let g:airline_powerline_fonts = 1  " arrowed blocks in status line
"let g:coc_global_extensions = [
    "\ 'coc-emmet',
    "\ 'coc-css',
    "\ 'coc-html',
    "\ 'coc-json',
    "\ 'coc-yaml',
    "\ 'coc-prettier',
    "\ 'coc-tsserver',
    "\ 'coc-snippets',
    "\ 'coc-pairs',
    "\ 'coc-elixir',
    "\ 'coc-tailwindcss',
    "\ 'coc-styled-components',
    "\]

nnoremap <leader>ut :UndotreeToggle<CR>
nnoremap <C-p> :GFiles<CR>
nnoremap <S-b> :Buffers<CR>

"" FZF integration with RipGrep -> :Rg search_pattern
let $FZF_DEFAULT_COMMAND = "rg --files --hidden --follow --glob '!.git'"
" Find and Replace recursively with Greppeg
" After searching for text, press this mapping to do a project wide find and
" replace. It's similar to <leader>r except this one applies to all matches
" across all files instead of just the current file.
nnoremap <Leader>R
  \ :let @s='\<'.expand('<cword>').'\>'<CR>
  \ :Grepper -cword -noprompt<CR>
  \ :cfdo %s/<C-r>s//g \| update
  \<Left><Left><Left><Left><Left><Left><Left><Left><Left><Left><Left>

nmap <leader>gd <Plug>(coc-defination)
nmap <leader>gr <Plug>(coc-references)

"" CoC Prettier
command! -nargs=0 Prettier :CocCommand prettier.formatFile
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
"" Coc Enable Disbale
nnoremap <leader>C :CocEnable<CR>
nnoremap <leader>D :CocDisable<CR>

"" Git Blamer
let g:blamer_delay = 500
let g:blamer_show_in_insert_modes = 0
let g:blamer_show_in_visual_modes = 0
let g:blamer_prefix = ' % '

"" IndentLine
let g:vim_json_conceal=0
let g:markdown_syntax_conceal=0

""source $HOME/.vim/nerdcommenter.vim
colorscheme gruvbox " 'NeoSolarized', 'nord', 'gruvbox' 'one'
highlight ColorColumn ctermbg=0 guibg=Grey40
set bg=dark  " 'dark', 'light'
set termguicolors "use RGB colors in TUI
let g:one_allow_italics = 1

"" Toggle NERDTree
nnoremap <silent> <leader>n :NERDTreeFocus<CR>
nnoremap <silent> <leader>m :NERDTreeClose<CR>
"" Exit Vim if NERDTree is the only window left.
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
"" Open the existing NERDTree on each new tab.
" autocmd BufWinEnter * silent NERDTreeMirror
let g:NERDTreeShowHidden = 1
let g:NERDTreeMinimalUI = 1
let g:NERDTreeIgnore = []
" enable line numbers
let NERDTreeShowLineNumbers=1
" make sure relative line numbers are used
autocmd FileType nerdtree setlocal relativenumber

let g:highlightedyank_highlight_duration = 350

" vim-test mappings
nmap <silent> t<C-n> :TestNearest<CR>
nmap <silent> t<C-f> :TestFile<CR>
nmap <silent> t<C-s> :TestSuite<CR>
nmap <silent> t<C-l> :TestLast<CR>
nmap <silent> t<C-v> :TestVisit<CR>
" make test commands execute using dispatch.vim
let test#strategy = "dispatch"

" rescript
"""""""""""
autocmd FileType rescript nnoremap <silent> <buffer> <leader>rf :RescriptFormat<CR>
autocmd FileType rescript nnoremap <silent> <buffer> <leader>rt :RescriptTypeHint<CR>
autocmd FileType rescript nnoremap <silent> <buffer> <leader>rb :RescriptBuild<CR>
autocmd FileType rescript nnoremap <silent> <buffer> gd :RescriptJumpToDefinition<CR>
" Hooking up the ReScript autocomplete function
set omnifunc=rescript#Complete
" " When preview is enabled, omnicomplete will display additional information for a selected item
set completeopt+=preview

