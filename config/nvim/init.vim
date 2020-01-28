call plug#begin(stdpath('data').'/plugged')
Plug 'dense-analysis/ale'
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

call matchadd('ColorColumn', '\%81v\S')

let g:ale_sign_error = "\uf06a"
let g:ale_sign_warning = "\uf071"

nnoremap <silent> <C-p> :FZF<CR>
nnoremap <silent> <Leader>af :ALEFix<CR>
nnoremap <silent> <Leader>an :ALENext<CR>
nnoremap <silent> <Leader>ap :ALEPrevious<CR>
