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
    Plug 'fatih/vim-go', { 'tag': '*' }
    Plug 'neoclide/coc.nvim', { 'branch': 'release' }
    Plug 'tpope/vim-surround'
    Plug 'jiangmiao/auto-pairs'
    Plug 'airblade/vim-gitgutter'
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
set lazyredraw
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
set scrolloff=10
set signcolumn=yes
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set autoindent
set si
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
au filetype go nmap <leader>t :w<CR>:GoTestFunc<CR>
au filetype go nmap <leader>T :w<CR>:GoTest<CR>
au filetype go nmap <leader>r :w<CR>:GoRun<CR>

" Omnisharp
let g:OmniSharp_popup_options = {
\ 'winblend': 30,
\ 'winhl': 'Normal:Normal'
\}

" CoC config
let g:coc_global_extensions = ['coc-emmet', 'coc-css', 'coc-html', 
  \ 'coc-json', 'coc-prettier', 'coc-tsserver']

" if hidden is not set, TextEdit might fail.
set hidden
" Better display for messages
set cmdheight=2
" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not
" mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" vim-go
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_operators = 1
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"
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
