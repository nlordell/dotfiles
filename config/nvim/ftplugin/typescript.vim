function! DenoProjectRoot(buffer) abort
    return expand('#' . a:buffer . ':p:h')
endfunction

execute ale#linter#Define('typescript', {
\   'name': 'deno',
\   'lsp': 'stdio',
\   'initialization_options': {
\       'enable': v:true,
\       'lint': v:true,
\       'unstable': v:true,
\   },
\   'executable': 'deno',
\   'command': '%e lsp -q',
\   'project_root': function('DenoProjectRoot'),
\})

function! DenoFmt(buffer) abort
    return { 'command': 'deno fmt -' }
endfunction

execute ale#fix#registry#Add(
\   'deno',
\   'DenoFmt',
\   ['typescript'],
\   'Deno formatter'
\)

if $NVIM_DENO != "off"
    let b:ale_linters = ['deno']
    let b:ale_fixers = ['deno']
else
    let b:ale_linters = ['eslint', 'tsserver']
    let b:ale_fixers = ['eslint']

    nnoremap <silent> <Leader>yb :!yarn build<CR>
    nnoremap <silent> <Leader>yt :!yarn test<CR>
endif

set expandtab shiftwidth=2
