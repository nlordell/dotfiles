let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')
let g:ale_rust_cargo_check_tests = 1
let b:ale_fixers = ['rustfmt']

nnoremap <silent> <Leader>cb :!cargo build<CR>
nnoremap <silent> <Leader>cc :!cargo clippy --workspace --all-targets<CR>
nnoremap <silent> <Leader>ct :!cargo test --workspace<CR>
nnoremap <silent> <Leader>cf :!cargo fmt --all<CR>
