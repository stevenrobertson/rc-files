set guifont=Dina\ 10
colorscheme ir_black
"set lines=49
set columns=83
set numberwidth=4
set guioptions-=T

if hostname() == "anubis"
    set guifont=Anonymous\ Pro\ 9
endif

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
