syntax on
set number
set numberwidth=3
set ignorecase
set smartcase
set vb
highlight comment ctermfg=blue
set guifont=Droid\ Sans\ Mono\ 9
set shiftwidth=4
set softtabstop=4
set tabstop=4
set textwidth=79
set expandtab
set hlsearch
set enc=utf-8
set fo+=2
inoremap <Home> <C-O>^
inoremap <C-A> <C-O>^
inoremap <C-E> <C-O>$
inoremap <C-H> <BS>
map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>

set modeline
set foldmethod=indent
set nofoldenable
set diffopt+=filler
set ruler
set laststatus=2

filetype on
filetype plugin indent on

" from http://blog.sontek.net/2008/05/11/python-with-a-modular-ide-vim/
set tags+=$HOME/.vim/tags/python.ctags
set tags+=$HOME/.vim/tags/current.ctags
autocmd FileType python set omnifunc=pythoncomplete#Complete
inoremap <C-space> <C-x><C-o>
inoremap <F5> <C-o>gqap
noremap <F5> gqap

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

function ToggleAutoformat()
    if &fo =~ "a"
        set fo-=a
    else
        set fo+=a
    endif
endfunction
inoremap <F6> <C-o>:call ToggleAutoformat()<CR>


vmap <Leader>/ : s:^  ://:<CR>

autocmd BufWritePre * :%s/\s\+$//e

function DoVimRun()
    let s:vrpath = findfile(".vimrun.sh", ".;")
    if s:vrpath != ""
        w
        let s:vrpath = fnamemodify(s:vrpath, ":p")
        call system("xterm -e 'zsh " . s:vrpath . "' &")
    endif
endfunction

inoremap <F7> <C-o>:w<CR>
noremap <F7> :w<CR>
inoremap <F8> <C-o>:call DoVimRun()<CR>
noremap <F8> :call DoVimRun()<CR>

