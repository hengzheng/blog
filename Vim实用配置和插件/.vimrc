" Specify a directory for plugins (for Neovim: ~/.local/share/nvim/plugged)
call plug#begin('~/.vim/plugged')

" airline
Plug 'vim-airline/vim-airline'

" erlang
Plug 'jimenezrick/vimerl'

" leaderf
Plug 'Yggdroot/LeaderF'

" minibufexplorepp
Plug 'vim-scripts/minibufexplorerpp'

" colorscheme
Plug 'morhetz/gruvbox'

" multiple-cursors
Plug 'terryma/vim-multiple-cursors'

" whitespace
Plug 'bronson/vim-trailing-whitespace'

" Initialize plugin system
call plug#end()


filetype plugin indent on
syntax on

"""""""""""""""""""""
" general setting.
"""""""""""""""""""""
" 关闭兼容
set nocompatible

" 空格代替tab
set expandtab

" 代替tab的空格数量
set tabstop=4

" 缩进的空格数
set shiftwidth=4

" 位于行首的tab代替成的空格用shiftwidth定义的数量(因为tabstop不一定跟shiftwidth相等)
set smarttab

" 显示行号
set number

" 设置状态栏
set laststatus=2
set wildmenu

" 当前文件在Vim外被编辑且当前Vim内未修改，则自动加载
set autoread

" 设置背景颜色
set background=dark

" 设置主题
" colorscheme desert
colorscheme gruvbox

" 列长度提醒
set colorcolumn=120
highlight ColorColumn ctermbg=6

" 搜索设置
" 搜索时忽略大小写
set ignorecase
" 如果搜索文本有大写，ignorecase则失效
set smartcase
" 搜索到头或到尾就结束，不循环
set nowrapscan
" 当输入搜索文本时，在文件中高亮匹配项
set incsearch
" 在文件中高亮前一个搜索的文本匹配项
set hlsearch

" 回到最后退出时光标的位置
if has("autocmd")
     autocmd! bufreadpost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

"""""""""""""""""""""""""""""""
" for minibufexplorerpp plugin.
"""""""""""""""""""""""""""""""
" 放下面
let g:miniBufExplSplitBelow = 1

"""""""""""""""""""""""""""""""
" for leaderF plugin.
"""""""""""""""""""""""""""""""
let g:Lf_WildIgnore = {
            \ 'dir': ['.svn','.git','.hg','ebin','proto','data*'],
            \ 'file': ['*.sw?','~$*','*.bak','*.exe','*.o','*.so','*.py[co]','.beam']
            \}

nmap <c-]> g<c-]>

set tags=tags
