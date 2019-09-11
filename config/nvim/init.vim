call plug#begin(stdpath('data').'/plugged')
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'raimondi/yaifa'
Plug 'terryma/vim-expand-region'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
call plug#end()

set inccommand=nosplit
set noshowmode
set number relativenumber
set list
" set listchars=tab:» ,trail:␣,nbsp:• " figure out why this isn't working
