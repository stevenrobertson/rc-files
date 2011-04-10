colorscheme ir_black
set guifont=Dina\ 11  columns=85 guioptions-=T

" lean back
func! LB()
    set guifont=Droid\ Sans\ Mono\ 13
endfunc

" vsplit, fix height
func! VS()
    set columns=166 | vsplit | set wiw=85
endfunc

inoremap <F11> <C-O>:set wiw=85<CR>
noremap <F11> :set wiw=85<CR>
inoremap <C-F11> <C-O>:set columns=85<CR>
noremap <C-F11> :set columns=85<CR>
set winaltkeys=no
