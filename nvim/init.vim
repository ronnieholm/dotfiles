" Partially originates from https://github.com/aqez/dotfiles/blob/master/nvim/init.vim
" https://www.youtube.com/watch?v=qGl_Mb2C87c

" Plugins
call plug#begin('~/.vim/plugged')
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    Plug 'morhetz/gruvbox'
    Plug 'tpope/vim-fugitive'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'preservim/nerdtree'
    Plug 'ntpeters/vim-better-whitespace'
    Plug 'puremourning/vimspector'
    Plug 'neovim/nvim-lspconfig'
    Plug 'nvim-lua/completion-nvim'
    Plug 'w0rp/ale'
    Plug 'OmniSharp/omnisharp-vim'
    Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'fatih/vim-go', { 'tag': '*' }
    Plug 'tpope/vim-surround'
    Plug 'jiangmiao/auto-pairs'
    Plug 'tomtom/tcomment_vim'
call plug#end()

" UI
colorscheme gruvbox
let g:gruvbox_contrast_dark = "hard"
hi! Normal guibg=NONE ctermbg=NONE
syntax on
set colorcolumn=81
set background=dark
set ruler
set backspace=eol,start,indent
set ignorecase
set smartcase
set incsearch
set nohlsearch
set magic
set showmatch
set foldcolumn=1
set number
set relativenumber
set completeopt=menuone,noinsert,noselect
set shortmess+=c
set previewheight=10
set laststatus=2
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set autoindent
"set si
set nowrap

" General
filetype plugin on
filetype indent on
let mapleader=" "
set clipboard=unnamedplus

" Files, backups and undo
set nobackup
set nowritebackup
set noswapfile
set hidden

" Customer key bindins
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
map <C-j> :cn<CR>
map <C-k> :cp<CR>
nnoremap <Leader>h <C-w>h
nnoremap <Leader>j <C-w>j
nnoremap <Leader>k <C-w>k
nnoremap <Leader>l <C-w>l

" Go specific bindings
" See https://github.com/fatih/vim-go-tutorial
" https://www.youtube.com/watch?v=7BqJ8dzygtU
au filetype go nmap <leader>t :w<CR>:GoTestFunc<CR>
au filetype go nmap <leader>T :w<CR>:GoTest<CR>
au filetype go nmap <leader>r :w<CR>:GoRun<CR>

" vim-better-whitespace
let g:strip_whitespace_on_save=1
let g:strip_whitespace_confirm=0
let g:strip_whitelines_at_eof=1

" Omnisharp
let g:OmniSharp_popup_options = {
\ 'winblend': 30,
\ 'winhl': 'Normal:Normal'
\}

" vim-go
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_operators = 1
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"
let g:go_auto_type_info = 1
let g:go_doc_popup_window = 1
au filetype go inoremap <buffer> . .<C-x><C-o>

" NerdTree
nnoremap <Leader>t :NERDTreeToggle<CR>
let g:NERDTreeQuitOnOpen = 0
let g:NERDTreeMinimalUI = 1
" Automaticaly close nvim if NERDTree is only thing left open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Vimspector
let g:vimspector_enable_mappings = 'VISUAL_STUDIO'

" FZF
nmap <Leader><Tab> <Plug>(fzf-maps-n)
nmap <Leader>p :Files<CR>

" LSP
lua << EOF
 local nvim_lsp = require('lspconfig')

 local on_attach = function(client, bufferNumber)
    require('completion').on_attach(client)
 end

 --local pid = vim.fn.getpid()
 --local omnisharp_bin = "/home/aqez/omnisharp/run"
 --nvim_lsp.omnisharp.setup({ cmd = { omnisharp_bin, "--languageserver" , "--hostPID", tostring(pid) } })
 nvim_lsp.rust_analyzer.setup({ on_attach = on_attach })
 nvim_lsp.clangd.setup({ on_attach = on_attach })
EOF

nnoremap gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <Leader>fi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <leader>cf <cmd>lua vim.lsp.buf.formatting()<CR>
nnoremap <leader><space> <cmd>lua vim.lsp.buf.code_action()<CR>
nnoremap <F2> <cmd>lua vim.lsp.buf.rename()<CR>
