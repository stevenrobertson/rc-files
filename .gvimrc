set guifont=Dina\ 11
colorscheme ir_black
"set lines=49
set columns=83
set numberwidth=4
set guioptions-=T

" lean back
func LB()
    set guifont=Droid\ Sans\ Mono\ 13
endfunc

" vsplit, fix height
func VS()
    set columns=166
    vsplit
    set wiw=80
endfunc
