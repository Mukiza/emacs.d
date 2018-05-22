call plug#begin('~/.local/share/nvim/plugged')

"" Terraform 
source "./lang/terraform.vim"

"" Common
Plug 'tpope/vim-fugitive'
Plug 'majutsushi/tagbar'
Plug 'vim-syntastic/syntastic'
Plug 'joshdick/onedark.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/denite.nvim', { 'do': 'UpdateRemotePlugins' }
Plug 'itchyny/lightline.vim'
call plug#end()
