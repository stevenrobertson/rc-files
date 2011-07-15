filetype off
call pathogen#runtime_append_all_bundles()
filetype plugin indent on

syntax on
highlight comment ctermfg=blue

set autoread number numberwidth=4 ignorecase smartcase vb ai
set shiftwidth=4 softtabstop=4 tabstop=4 textwidth=78 expandtab
set hlsearch enc=utf-8 fo+=2l bs=2
set modeline
set foldmethod=indent nofoldenable diffopt+=filler
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P%{AfCheck()}
set laststatus=2

function! ToggleAutoformat()
    if &fo =~ "a" | setl fo-=a fo+=r | else | setl fo+=a fo-=r | endif
endfunction
function! AfCheck()
    if &fo =~ "a" | return " A" | else | return "" | endif
endfunction

inoremap <Home> <C-O>^
inoremap <C-A> <C-O>^
inoremap <C-E> <C-O>$
noremap <C-A> ^
noremap <C-E> $
inoremap <C-H> <BS>
map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>

inoremap kj <Esc>
inoremap <A-h> <C-o>h
inoremap <A-j> <C-o>j
inoremap <A-k> <C-o>k
inoremap <A-l> <C-o>l

inoremap <F7> <C-o>:w<CR>
noremap <F7> :w<CR>
inoremap <F9> <C-o>gqap
noremap <F9> gqap
inoremap <F10> <C-o>:call ToggleAutoformat()<CR>
noremap <F10> :call ToggleAutoformat()<CR>

let NERDTreeQuitOnOpen=1
let NERDTreeWinPos="right"
let NERDTreeShowBookmarks=1
let NERDTreeHijackNetrw=1
let NERDTreeSortOrder=['\/$', '\.hs$', '\.py$', '*', '\.swp$',  '\.bak$', '\~$']
inoremap <F12> <Esc>:NERDTreeToggle<CR>
noremap <F12> :NERDTreeToggle<CR>
inoremap <C-F12> <C-o>:tabnew<CR>
noremap <C-F12> :tabnew<CR>

" from http://blog.sontek.net/2008/05/11/python-with-a-modular-ide-vim/
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType python set tags+=$HOME/.vim/tags/python.ctags
autocmd FileType python set tags+=$HOME/.vim/tags/current.ctags
inoremap <C-space> <C-x><C-o>

let g:haddock_browser="/usr/bin/firefox"
"au Bufenter *.hs compiler ghc
let g:hs_allow_hash_operator=1

autocmd BufWritePre * :%s/\s\+$//e
command! Hgd diffthis | let res = system('~/.scripts/pickfromdiff.py ' . shellescape(expand('%'))) | vert new | set bt=nofile | put=res | diffthis
command! Goo set sw=2 sts=2 ts=2

