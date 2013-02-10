filetype off
call pathogen#infect()
filetype plugin indent on

if filereadable("/usr/share/vim/google/google.vim")
    source /usr/share/vim/google/google.vim
    set nolist
endif

syntax on
highlight comment ctermfg=blue

set autoread number numberwidth=4 ignorecase smartcase vb ai
set shiftwidth=4 softtabstop=4 tabstop=4 textwidth=78 expandtab
set hlsearch enc=utf-8 fo+=2l bs=2
set modeline
set foldmethod=indent nofoldenable diffopt+=filler,vertical
" set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P%{AfCheck()}
set laststatus=2
set wildmode=longest,list,full wildmenu
set mouse=a ttymouse=xterm2

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
vnoremap <S-j> j
vnoremap <S-k> k

inoremap    <F7>  <C-o>:w<CR>
noremap     <F7>       :w<CR>
inoremap    <F9>  <C-o>gqap
noremap     <F9>       gqap
inoremap    <F10> <C-o>:call ToggleAutoformat()<CR>
noremap     <F10>      :call ToggleAutoformat()<CR>

" FSSwitch
nmap <silent> <Leader>of :FSHere<cr>
nmap <silent> <Leader>ol :FSRight<cr>
nmap <silent> <Leader>oL :FSSplitRight<cr>
nmap <silent> <Leader>oh :FSLeft<cr>
nmap <silent> <Leader>oH :FSSplitLeft<cr>
nmap <silent> <Leader>ok :FSAbove<cr>
nmap <silent> <Leader>oK :FSSplitAbove<cr>
nmap <silent> <Leader>oj :FSBelow<cr>
nmap <silent> <Leader>oJ :FSSplitBelow<cr>

au! BufEnter *.{c,cc} let b:fswitchdst = 'h' | let b:fswitchlocs = '.'
au! BufEnter *.{h} let b:fswitchdst = 'cc,c' | let b:fswitchlocs = '.'
au! BufEnter *.cpp let b:fswitchdst = 'hpp,h' | let b:fswitchlocs = '.'
au! BufEnter *.hpp let b:fswitchdst = 'hpp' | let b:fswitchlocs = '.'

let NERDTreeQuitOnOpen=1
let NERDTreeWinPos="right"
let NERDTreeShowBookmarks=1
let NERDTreeHijackNetrw=1
let NERDTreeSortOrder=['\/$', '\.hs$', '\.py$', '*', '\.swp$',  '\.bak$', '\~$']
inoremap <F12> <Esc>:NERDTreeToggle<CR>
noremap <F12> :NERDTreeToggle<CR>
inoremap <C-F12> <C-o>:tabnew<CR>
noremap <C-F12> :tabnew<CR>

let g:vimclojure#ParenRainbow=1
let g:vimclojure#FuzzyIndent=1
let g:vimclojure#WantNailgun=1
let g:vimclojure#FuzzyIndentPatterns .= ",.*"

" autocmd BufReadPost * let b:had_whitespace = match(getline(0, "$"), '\s\+$')
" autocmd BufWritePre * if ! exists("b:had_whitespace") || b:had_whitespace == -1 | %s/\s\+$//e | endif
autocmd BufWritePre * %s/\s\+$//e

command! Hgd diffthis | let res = system('~/.scripts/pickfromdiff.py ' . shellescape(expand('%'))) | vert new | set bt=nofile | put=res | diffthis
set diffopt=vertical
command! Goo set sw=2 sts=2 ts=2

if &term =~ '^screen'
    set t_ts=^[k
    set t_fs=^[\
    set term=xterm-color
endif

" for CSApprox
if &term =~ '^xterm' || &term =~ '^rxvt'
    set title
    set t_Co=256
    colorscheme ir_black
endif

" Don't use swapfile when started in diff mode (handy for 'git mergetool')
if &diff
  set uc=0
endif

" Don't leave an exit message
let &titleold=''

" Powerline theme
let g:Powerline_stl_path_style='short'
call Pl#Theme#RemoveSegment('virtualenv:statusline')
call Pl#Theme#RemoveSegment('fileformat')
call Pl#Theme#RemoveSegment('fileencoding')
call Pl#Theme#RemoveSegment('filetype')
call Pl#Theme#RemoveSegment('scrollpercent')
