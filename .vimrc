" 1 Plugin region
" 2 Airline theme
" 3 NERDTree settings
" 4 General vim settings
" 5 Macros and keybindings

" 1) PLUGIN REGION
call plug#begin('~/.config/nvim/plugged')

Plug 'tpope/vim-surround'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'valloric/youcompleteme'
Plug 'slashmili/alchemist.vim'
Plug 'elixir-editors/vim-elixir'
Plug 'matze/vim-move'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'mattn/emmet-vim'
call plug#end()

" 2) Airline theme
let g:airline_theme='base16_gruvbox_dark_hard'
let g:airline_powerline_fonts = 1

" 3) NERDTree settings
" Autostart NERDTree if no file is specified in the vim command
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
let NERDTreeQuitOnOpen=1
let NERDTreeShowHidden=1

let g:user_emmet_settings = {
\  'html' : {
\    'indent_blockelement': 1,
\  },
\}

" 4) General vim settings
:set number
:set hlsearch
:set ic
:set tabstop=4
:set shiftwidth=4
:set expandtab

" 5) Macros & custom keybindings
fun! ToggleNERDTreeWithRefresh()
	:NERDTreeToggle 
	if(exists("b:NERDTreeType") == 1)
        	call feedkeys("R")  
	endif
endf


let g:move_key_modifier = 'C'
:let @c='"+y'
:let @v='"+p'
nmap <silent> <C-e> :call ToggleNERDTreeWithRefresh()<CR>
map <C-c> @c<CR>
map <C-v> @v<CR>
