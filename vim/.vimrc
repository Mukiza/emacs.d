" vimconf is not vi-compatible
set nocompatible

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/bundle')

""" Syntax highlighting {{{
syntax on
filetype plugin indent on                   " detect file plugin+indent
set t_Co=256                                " 256-colors
set background=dark                         " we're using a dark bg
colors jellybeans                           " select colorscheme
highlight Normal ctermbg=NONE               " use terminal background
highlight nonText ctermbg=NONE              " use terminal background
highlight LineNr ctermbg=NONE               " use terminal background
highlight SignColumn ctermbg=NONE           " use terminal background
highlight CursorLine ctermbg=235            " a slightly lighter line
au BufRead,BufNewFile *.txt set ft=sh       " opens .txt w/highlight

""" Tab colors, overwritten by lightline(?) {{{
    "hi TabLineFill ctermfg=NONE ctermbg=233
    "hi TabLine ctermfg=241 ctermbg=233
    "hi TabLineSel ctermfg=250 ctermbg=233

set cursorline                              " hilight cursor line
set more                                    " ---more--- like less
set number                                  " line numbers
set scrolloff=3                             " lines above/below cursor
set showcmd                                 " show cmds being typed
set title                                   " window title
set vb t_vb=                                " disable beep and flashing
set wildignore=.bak,.pyc,.o,.ojb,.,a,       " ignore said files
                \.pdf,.jpg,.gif,.png,
                \.avi,.mkv,.so
set wildmenu                                " better auto complete
set wildmode=longest,list                   " bash-like auto complete
" set encoding=utf-8                        " for character glyphs

""" General settings
set hidden                                      " buffer change, more undo
set history=1000                                " default 20
set iskeyword+=_,$,@,%,#                        " not word dividers
set laststatus=2                                " always show statusline
set linebreak                                   " don't cut words on wrap
set listchars=tab:>\                            " > to highlight <tab>
set list                                        " displaying listchars
set mouse=                                      " disable mouse
set relativenumber                              " enable relative numbers
set nolist                                      " wraps to whole words
set noshowmode                                  " hide mode cmd line
set noexrc                                      " don't use other .*rc(s)
set nostartofline                               " keep cursor column pos
set nowrap                                      " don't wrap lines
set numberwidth=5                               " 99999 lines
set shortmess+=I                                " disable startup message
set splitbelow                                  " splits go below w/focus
set splitright                                  " vsplits go right w/focus
set ttyfast                                     " for faster redraws etc
set ttymouse=xterm2                             " experimental

""" Folding
set foldcolumn=0                            " hide folding column
set foldmethod=indent                       " folds using indent
set foldnestmax=10                          " max 10 nested folds
set foldlevelstart=99                       " folds open by default

""" Search and replace
set gdefault                                " default s//g (global)
set incsearch                               " "live"-search

""" Matching
set matchtime=2                             " time to blink match {}
set matchpairs+=<:>                         " for ci< or ci>
set showmatch                               " tmpjump to match-bracket

""" Return to last edit position when opening files {{{
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \     exe "normal! g`\"" |
    \ endif

""" Files
set autochdir                                   " always use curr. dir.
set autoread                                    " refresh if changed
set confirm                                     " confirm changed files
set noautowrite                                 " never autowrite
set nobackup                                    " disable backups
set updatecount=50                              " update swp after 50chars
""" Persistent undo. Requires Vim 7.3 {{{
if has('persistent_undo') && exists("&undodir")
    set undodir=$HOME/.vim/undo/            " where to store undofiles
    set undofile                            " enable undofile
    set undolevels=500                      " max undos stored
    set undoreload=10000                    " buffer stored undos
endif

""" Text formatting
set autoindent                                  " preserve indentation
set backspace=indent,eol,start                  " smart backspace
set cinkeys-=0#                                 " don't force # indentation
set expandtab                                   " no real tabs
set ignorecase                                  " by default ignore case
set nrformats+=alpha                            " incr/decr letters C-a/-x
set shiftround                                  " be clever with tabs
set shiftwidth=4                                " default 8
set smartcase                                   " sensitive with uppercase
set smarttab                                    " tab to 0,4,8 etc.
set softtabstop=4                               " "tab" feels like <tab>
set tabstop=4                                   " replace <TAB> w/4 spaces

""" Only auto-comment newline for block comments
au FileType c,cpp setlocal comments -=:// comments +=f://

""" Keybindings
" Remap <leader>
let mapleader=","

" Quickly edit/source .vimrc
noremap <leader>ve :edit $HOME/.vimrc<CR>
noremap <leader>vs :source $HOME/.vimrc<CR>

" Yank(copy) to system clipboard
noremap <leader>y "+y

" Toggle text wrapping
nmap <silent> <leader>w :set invwrap<CR>:set wrap?<CR>
nmap <silent> <C-L> :PyFlakeAuto<CR>
" Toggle folding
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf

nnoremap <F5> :<C-U>make %:r && ./%:r<CR>

" Bubbling (bracket matching)
nmap <C-up> [e
nmap <C-down> ]e
vmap <C-up> [egv
vmap <C-down> ]egv

" Move faster
map <C-j> <C-d>
map <C-k> <C-u>

" Treat wrapped lines as normal lines
nnoremap j gj
nnoremap k gk

" Working ci(, works for both breaklined, inline and multiple ()
nnoremap ci( %ci(

" We don't need any help!
inoremap <F1> <nop>
nnoremap <F1> <nop>
vnoremap <F1> <nop>

" Disable annoying ex mode (Q)
map Q <nop>

" Buffers, preferred over tabs now with bufferline. Buggy?
nnoremap nb :bNext<CR>
nnoremap pb :bprevious<CR>
nnoremap xb :bdelete<CR>
nnoremap fb <C-^>

" Ctrl Left & Right move between buffers
noremap <silent> <C-h> :bprev<CR>
noremap <silent> <C-l> :bnext<CR>

" Extra 'clipboard' register
nnoremap <leader>d "_d
vnoremap <leader>d "_d
vnoremap <leader>p "_dP
""" Toggle syntax highlighting
map <F4> :if exists("syntax_on")
    \<Bar>syntax off<Bar>else<Bar>syntax enable<Bar>endif<CR>

""" Highlight characters past 79, toggle with <leader>h
let g:overlength_enabled = 0
highlight OverLength ctermbg=black guibg=#212121

function! ToggleOverLengthHighlight()
    if g:overlength_enabled == 0
        match OverLength /\%79v.*/
        let g:overlength_enabled = 1
        echo 'OverLength highlighting turned on'
    else
        match
        let g:overlength_enabled = 0
        echo 'OverLength highlighting turned off'
    endif
endfunction

nnoremap <leader>h :call ToggleOverLengthHighlight()<CR>

""" Toggle relativenumber using <leader>r
nnoremap <leader>r :call NumberToggle()<CR>

function! NumberToggle()
    if(&relativenumber == 1)
        set number
    else
        set relativenumber
    endif
endfunction

""" Remove multiple empty lines
function! DeleteMultipleEmptyLines()
    g/^\_$\n\_^$/d
endfunction

nnoremap <leader>ld :call DeleteMultipleEmptyLines()<CR>

""" Split to relative header/source
function! SplitRelSrc()
    let s:fname = expand("%:t:r")

    if expand("%:e") == "h"
        set nosplitright
        exe "vsplit" fnameescape(s:fname . ".cpp")
        set splitright
    elseif expand("%:e") == "cpp"
        exe "vsplit" fnameescape(s:fname . ".h")
    endif
endfunction

nnoremap <leader>le :call SplitRelSrc()<CR>

""" Strip trailing whitespace, return to cursors at save
function! <SID>StripTrailingWhitespace()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfunction

autocmd FileType php,cpp,py,js,html,hs,sh,c,perl,ts  autocmd
            \ BufWritePre <buffer> :call <SID>StripTrailingWhitespace()


" Toggle pastemode, doesn't indent
set pastetoggle=<F3>

" Syntastic - toggle error list. Probably should be toggleable.
noremap <silent><leader>lo :Errors<CR>
noremap <silent><leader>lc :lcl<CR>

""" Lightline
let g:lightline = {
    \ 'colorscheme': 'jellybeans',
    \ 'active': {
    \     'left': [
    \         ['mode', 'paste'],
    \         ['fugitive'],
    \         ['ctrlpmark', 'bufferline']
    \     ],
    \     'right': [
    \         ['lineinfo'],
    \         ['percent'],
    \         ['fileformat', 'fileencoding', 'filetype', 'syntastic']
    \     ]
    \ },
    \ 'component': {
    \     'paste': '%{&paste?"!":""}',
    \     'bufferline': '%{bufferline#refresh_status()}%{g:bufferline_status_info.before . g:bufferline_status_info.current . g:bufferline_status_info.after}'
    \ },
    \ 'component_function': {
    \     'filename'     : 'MyFilename',
    \     'fileformat'   : 'MyFileformat',
    \     'filetype'     : 'MyFiletype',
    \     'fileencoding' : 'MyFileencoding',
    \     'mode'         : 'MyMode',
    \     'fugitive'     : 'MyFugitive',
    \     'ctrlpmark'    : 'CtrlPMark'
    \ },
    \ 'component_expand': {
    \     'syntastic': 'SyntasticStatuslineFlag',
    \ },
    \ 'component_type': {
    \     'syntastic': 'middle',
    \ },
    \ 'subseparator': {
    \     'left': '|', 'right': '|'
    \ }
    \ }

let g:lightline.mode_map = {
    \ 'n'      : ' N ',
    \ 'i'      : ' I ',
    \ 'R'      : ' R ',
    \ 'v'      : ' V ',
    \ 'V'      : 'V-L',
    \ 'c'      : ' C ',
    \ "\<C-v>" : 'V-B',
    \ 's'      : ' S ',
    \ 'S'      : 'S-L',
    \ "\<C-s>" : 'S-B',
    \ '?'      : '      ' }

function! MyFileformat()
    return winwidth('.') > 80 ? &fileformat : ''
endfunction

function! MyFiletype()
    return winwidth('.') > 80 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! MyFileencoding()
    return winwidth('.') > 80 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! MyMode()
    let fname = expand('%:t')
    return fname == '__Tagbar__' ? 'Tagbar' :
            \ fname == 'ControlP' ? 'CtrlP' :
            \ winwidth('.') > 60 ? lightline#mode() : ''
endfunction

function! MyFugitive()
    try
        if expand('%:t') !~? 'Tagbar' && exists('*fugitive#head')
            let mark = '∓ '
            let _ = fugitive#head()
            return strlen(_) ? mark._ : ''
        endif
    catch
    endtry
    return ''
endfunction

function! CtrlPMark()
    if expand('%:t') =~ 'ControlP'
        call lightline#link('iR'[g:lightline.ctrlp_regex])
        return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
            \ , g:lightline.ctrlp_next], 0)
    else
        return ''
    endif
endfunction

let g:ctrlp_status_func = {
    \ 'main': 'CtrlPStatusFunc_1',
    \ 'prog': 'CtrlPStatusFunc_2',
    \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
    let g:lightline.ctrlp_regex = a:regex
    let g:lightline.ctrlp_prev = a:prev
    let g:lightline.ctrlp_item = a:item
    let g:lightline.ctrlp_next = a:next
    return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
    return lightline#statusline(0)
endfunction

let g:tagbar_status_func = 'TagbarStatusFunc'

function! TagbarStatusFunc(current, sort, fname, ...) abort
    let g:lightline.fname = a:fname
    return lightline#statusline(0)
endfunction

augroup AutoSyntastic
    autocmd!
    autocmd BufWritePost *.hs,*.c,*.cpp,*.perl,*py call s:syntastic()
augroup END

function! s:syntastic()
    SyntasticCheck
    call lightline#update()
endfunction

" clang_complete - C++11
let g:clang_user_options="-std=c++0x"

" CtrlP - don't recalculate files on start (slow)
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_working_path_mode = 'rc'
let g:ctrlp_match_window = 'min:1,max:70,results=100'

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|svn)$',
  \ 'file': '\v\.(so|dll|pyc|swo|swp)$',
  \ }

" TagBar
let g:tagbar_left = 0
let g:tagbar_width = 30
set tags=tags;/

"Rails
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType php set omnifunc=phpcomplete#CompletePHP

autocmd BufNewFile,BufRead *_spec.rb compiler rspec
autocmd BufNewFile,BufRead Gemfile set filetype=ruby
autocmd BufNewFile,BufRead *.hiccup set filetype=clojure
autocmd BufRead,BufNewFile *.cljs setlocal filetype=clojure
autocmd FileType ruby,eruby set filetype=ruby.eruby.chef

" Syntastic -
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_compiler_options = ' -std=c++0x'
let g:syntastic_mode_map = {
    \ 'mode': 'passive',
    \ 'active_filetypes':
        \ ['hs', 'php', 'c', 'cpp', 'perl', 'python'] }

" Automatically remove preview window after autocomplete (mainly for clang_complete)
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

""" Local ending config, will overwrite anything above. Generally use this
if filereadable($HOME."/.vimrc.bundles")
    source $HOME/.vimrc.bundles
endif

let g:PyFlakeOnWrite = 1
let g:PyFlakeCheckers = 'pep8,mccabe,pyflakes'
let g:PyFlakeCWindow = 6

let g:vimclojure#HighlightBuiltins = 1
let g:vimclojure#ParenRainbow = 1

autocmd FileType clojure nnoremap <silent><buffer> cpr :Require \| ClojureHighlightReferences<CR>

map <F8> :w<CR> :!clear; make<CR> :!./%<<CR>

nmap <F11> :TagbarToggle<CR>
nmap <F2> :NERDTreeToggle<CR>
"let g:NERDTreeDirArrows=0
let NERDTreeIgnore = ['\.pyc$']


let g:rspec_command = 'call Send_to_Tmux("rspec {spec}\n")'
let mapleader=","

map <F7> :w <CR> :!gcc % -o %< && ./%< <CR>
map <Leader>t :call RunCurrentSpecFile()<CR>
map <Leader>s :call RunNearestSpec()<CR>
map <Leader>l :call RunLastSpec()<CR>
map <Leader>a :call RunAllSpecs()<CR>

let g:rspec_runner = "os_x_iterm"

"------  Fugitive  ------
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gr :Gremove<CR>
nnoremap <Leader>gl :Glog<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gm :Gmove
nnoremap <Leader>gp :Ggrep
nnoremap <Leader>gR :Gread<CR>
nnoremap <Leader>gg :Git
nnoremap <Leader>gd :Gdiff<CR>

call plug#end()
