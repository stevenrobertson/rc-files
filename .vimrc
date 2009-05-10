set number
set numberwidth=3
set ignorecase
set smartcase
highlight comment ctermfg=blue
set guifont=Droid\ Sans\ Mono\ 8
set softtabstop=4
set shiftwidth=4
set expandtab
set textwidth=79
inoremap <Home> <C-O>^
inoremap <C-A> <C-O>^
inoremap <C-E> <C-O>$
set modeline
set foldmethod=indent
set nofoldenable
set diffopt=filler
set ruler
set laststatus=2

filetype on
filetype indent on

" from http://blog.sontek.net/2008/05/11/python-with-a-modular-ide-vim/
set tags+=$HOME/.vim/tags/python.ctags
autocmd FileType python set omnifunc=pythoncomplete#Complete
inoremap <Nul> <C-x><C-o>

if hostname() == "tantalus"
    set dir=/mnt/offload/vim-swap
    set backupdir=/mnt/offload/vim-swap
endif

syntax match Error /\s*$/

if has("gui_running")
    colorscheme ir_black
    set columns=83
endif

setlocal wrap linebreak nolist
set virtualedit=
setlocal display+=lastline
noremap  <buffer> <silent> <Up>   gk
noremap  <buffer> <silent> <Down> gj
noremap  <buffer> <silent> <Home> g<Home>
noremap  <buffer> <silent> <End>  g<End>
inoremap <buffer> <silent> <Up>   <C-o>gk
inoremap <buffer> <silent> <Down> <C-o>gj
inoremap <buffer> <silent> <Home> <C-o>g<Home>
inoremap <buffer> <silent> <End>  <C-o>g<End>

noremap <silent> <Leader>w :call ToggleWrap()<CR>
function ToggleWrap()
  if &wrap
    echo "Wrap OFF"
    setlocal nowrap
    set virtualedit=all
    silent! nunmap <buffer> <Up>
    silent! nunmap <buffer> <Down>
    silent! nunmap <buffer> <Home>
    silent! nunmap <buffer> <End>
    silent! iunmap <buffer> <Up>
    silent! iunmap <buffer> <Down>
    silent! iunmap <buffer> <Home>
    silent! iunmap <buffer> <End>
  else
    echo "Wrap ON"
    setlocal wrap linebreak nolist
    set virtualedit=
    setlocal display+=lastline
    noremap  <buffer> <silent> <Up>   gk
    noremap  <buffer> <silent> <Down> gj
    noremap  <buffer> <silent> <Home> g<Home>
    noremap  <buffer> <silent> <End>  g<End>
    inoremap <buffer> <silent> <Up>   <C-o>gk
    inoremap <buffer> <silent> <Down> <C-o>gj
    inoremap <buffer> <silent> <Home> <C-o>g<Home>
    inoremap <buffer> <silent> <End>  <C-o>g<End>
  endif
endfunction

