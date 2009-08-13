set guifont=Droid\ Sans\ Mono\ 9

if hostname() == "hermes"
    set guifont=Droid\ Sans\ Mono\ 8
endif

if hostname() == "anubis"
    set guifont=Anonymous\ Pro\ 9
endif

if has("gui_running")
    colorscheme ir_black
    set columns=83
    set guioptions=-T
endif

