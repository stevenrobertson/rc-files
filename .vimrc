syntax on
set autoread
set number
set numberwidth=4
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
set fo+=2nl
inoremap <Home> <C-O>^
inoremap <C-A> <C-O>^
inoremap <C-E> <C-O>$
noremap <C-A> ^
noremap <C-E> $
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

function! ToggleAutoformat()
    if &fo =~ "a"
        set fo-=a
    else
        set fo+=a
    endif
endfunction
inoremap <F9> <C-o>:call ToggleAutoformat()<CR>
noremap <F9> :call ToggleAutoformat()<CR>

autocmd BufWritePre * :%s/\s\+$//e

function! DoVimRun(arg)
    let s:vrpath = findfile(".vimrun.sh", ".;")
    if s:vrpath != ""
        w
        let s:vrpath = fnamemodify(s:vrpath, ":p")
        call system("xterm -e 'zsh " . s:vrpath . a:arg . "' &")
    endif
endfunction

inoremap <F7> <C-o>:w<CR>
noremap <F7> :w<CR>
inoremap <F8> <C-o>gqap
noremap <F8> gqap

"inoremap <F8> <C-o>:call DoVimRun("")<CR>
"noremap <F8> :call DoVimRun("")<CR>
"inoremap <C-F8> <C-o>:call DoVimRun(" 1")<CR>
"noremap <C-F8> :call DoVimRun(" 1")<CR>

au BufEnter *.hs compiler ghc
let g:haddock_browser="/usr/bin/firefox"

" aw, hell.
inoremap kj <Esc>
inoremap <Left>  <NOP>
inoremap <Right> <NOP>
inoremap <Up>    <NOP>
inoremap <Down>  <NOP>
noremap <Left>  <NOP>
noremap <Right> <NOP>
noremap <Up>    <NOP>
noremap <Down>  <NOP>

inoremap <A-h> <C-o>h
inoremap <A-j> <C-o>j
inoremap <A-k> <C-o>k
inoremap <A-l> <C-o>l

let NERDTreeQuitOnOpen=1
let NERDTreeWinPos="right"
let NERDTreeShowBookmarks=1
inoremap <F12> <C-o>:NERDTreeToggle<CR>
noremap <F12> :NERDTreeToggle<CR>

