colorscheme ir_black
set guifont=Dina\ 11 columns=83 guioptions=agim

" lean back
func! LB()
    set guifont=Droid\ Sans\ Mono\ 13
endfunc

" vsplit, fix height
func! VS()
    set columns=166 | vsplit | set wiw=83
endfunc

inoremap <F11> <C-O>:set wiw=83<CR>
noremap <F11> :set wiw=83<CR>
inoremap <C-F11> <C-O>:set columns=83<CR>
noremap <C-F11> :set columns=83<CR>
set winaltkeys=no
