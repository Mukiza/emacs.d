"Load all plugins
source './plugins.vim'

syntax on
filetype plugin indent on

" Syntastic Config
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" (Optional)Remove Info(Preview) window
set completeopt-=preview

" (Optional)Hide Info(Preview) window after completions
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" (Optional) Enable terraform plan to be include in filter
let g:syntastic_terraform_tffilter_plan = 1

" (Optional) Default: 0, enable(1)/disable(0) plugin's keymapping
let g:terraform_completion_keys = 1

" (Optional) Default: 1, enable(1)/disable(0) terraform module registry completion
let g:terraform_registry_module_completion = 0

let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.terraform = '[^ *\t"{=$]\w*'
let g:deoplete#enable_at_startup = 1
call deoplete#initialize()

""" Syntax highlighting {{{
set t_Co=256                                " 256-colors
set background=dark                         " we're using a dark bg
colorscheme onedark
highlight Normal ctermbg=NONE               " use terminal background
highlight nonText ctermbg=NONE              " use terminal background
highlight LineNr ctermbg=NONE               " use terminal background
highlight SignColumn ctermbg=NONE           " use terminal background
highlight CursorLine ctermbg=235            " a slightly lighter line
au BufRead,BufNewFile *.txt set ft=sh       " opens .txt w/highlight

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

" Yank(copy) to system clipboard
noremap <leader>y "+y

" Toggle text wrapping
nmap <silent> <leader>w :set invwrap<CR>:set wrap?<CR>

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

""" Remove multiple empty lines
function! DeleteMultipleEmptyLines()
    g/^\_$\n\_^$/d
endfunction

nnoremap <leader>ld :call DeleteMultipleEmptyLines()<CR>

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

augroup AutoSyntastic
    autocmd!
    autocmd BufWritePost *.hs,*.c,*.cpp,*.perl,*py call s:syntastic()
augroup END

function! s:syntastic()
    SyntasticCheck
    call lightline#update()
endfunction

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
