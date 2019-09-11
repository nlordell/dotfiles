call plug#begin(stdpath('data').'/plugged')
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
call plug#end()

set inccommand=nosplit
set noshowmode
set number relativenumber
set list
set listchars=tab:»\ ,nbsp:␣,trail:·
set autoindent copyindent
set noexpandtab tabstop=4 shiftwidth=4 smarttab
