let g:polyglot_disabled = ['autoindent', 'sensible']

call plug#begin(stdpath('cache').'/plugged')
Plug 'dense-analysis/ale'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'sheerun/vim-polyglot'
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

call matchadd('ColorColumn', '\%81v.')

let g:ale_sign_error = "\uf06a"
let g:ale_sign_warning = "\uf071"

nnoremap <silent> <Leader>af :ALEFix<CR>
nnoremap <silent> <Leader>an :ALENext<CR>
nnoremap <silent> <Leader>ap :ALEPrevious<CR>
nnoremap <silent> <Leader>gm :Rg =======<CR>
nnoremap <silent> <Leader>fb :Buffers<CR>
nnoremap <silent> <Leader>ff :Files<CR>
nnoremap <silent> <Leader>fg :GFiles<CR>
nnoremap <silent> <Leader>fl :Lines<CR>
