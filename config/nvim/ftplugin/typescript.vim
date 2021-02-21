if $NVIM_DENO != 'off'
    let b:ale_linters = ['deno']
    let b:ale_fixers = ['deno']
else
    let b:ale_linters = ['eslint', 'tsserver']
    let b:ale_fixers = ['eslint', 'prettier']

    nnoremap <silent> <Leader>yb :!yarn build<CR>
    nnoremap <silent> <Leader>yt :!yarn test<CR>
endif

set expandtab shiftwidth=2
