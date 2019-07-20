filetype off
call pathogen#infect()
filetype plugin indent on
set t_Co=256

if filereadable("/usr/share/vim/google/google.vim")
    source /usr/share/vim/google/google.vim
    set nolist
endif

syntax on
highlight comment ctermfg=blue
set background=dark

set autoread number numberwidth=4 ignorecase smartcase vb ai
set shiftwidth=4 softtabstop=4 tabstop=4 textwidth=78 expandtab
set hlsearch enc=utf-8 fo+=2l bs=2
set modeline
set foldmethod=indent nofoldenable diffopt+=filler,vertical
" set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P%{AfCheck()}
set laststatus=2
set wildmode=longest,list,full wildmenu
set mouse= ttymouse= notitle
set viminfo+=!

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

au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
let g:rbpt_colorpairs = [
  \ ['green', 'green'],
  \ ['yellow', 'yellow'],
  \ ['cyan', 'cyan'],
  \ ['magenta', 'magenta'],
  \ ['red', 'red'],
  \ ['green', 'green'],
  \ ['yellow', 'yellow'],
  \ ['cyan', 'cyan'],
  \ ['magenta', 'magenta'],
  \ ['red', 'red'],
  \ ['green', 'green'],
  \ ['yellow', 'yellow'],
  \ ['cyan', 'cyan'],
  \ ['magenta', 'magenta'],
  \ ['red', 'red'],
  \ ['green', 'green'],
  \ ['yellow', 'yellow'],
  \ ['cyan', 'cyan'],
  \ ['magenta', 'magenta']
  \ ]
let g:rbpt_max=19

autocmd BufWritePre * %s/\s\+$//e

set diffopt=vertical
command! Goo set sw=2 sts=2 ts=2

colorscheme ir_black

" Don't use swapfile when started in diff mode (handy for 'git mergetool')
if &diff
  set uc=0
endif
