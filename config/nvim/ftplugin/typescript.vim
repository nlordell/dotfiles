" TODO(nlordell): The Deno LSP seems to not _quite_ be working with ALE right
" now. Specifically, it is expecting settings to be sent on initialization
" which ALE does not do.
"
" function! DenoProjectRoot(buffer) abort
"     return expand('#' . a:buffer . ':p:h')
" endfunction
" execute ale#linter#Define('typescript', {
" \   'name': 'denolsp',
" \   'lsp': 'stdio',
" \   'lsp_config': {
" \       'enable': v:true,
" \       'lint': v:true,
" \       'unstable': v:false,
" \   },
" \   'executable': 'deno',
" \   'command': '%e lsp -q',
" \   'project_root': function('DenoProjectRoot'),
" \})

function! DenoFmt(buffer) abort
    return { 'command': 'deno fmt -' }
endfunction

execute ale#fix#registry#Add(
\   'denofmt',
\   'DenoFmt',
\   ['typescript'],
\   'Deno formatter'
\)

" let b:ale_linters = ['denolsp']
let b:ale_linters = []
let b:ale_fixers = ['denofmt']

set expandtab shiftwidth=2
