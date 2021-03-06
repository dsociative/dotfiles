set nocompatible               " Be iMproved

" NeoBundle check and auto install
let iCanHazNeoBundle=1
let NeoBundle_readme=expand('~/.vim/bundle/neobundle.vim/README.md')
if !filereadable(NeoBundle_readme)
    echo "Installing NeoBundle.."
    echo ""
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim
    let iCanHazNeoBundle=0
endif

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
NeoBundle 'Shougo/vimproc', {'build': {'unix': 'make -f make_unix.mak'}}
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neoyank.vim'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'fatih/vim-go'
NeoBundle 'klen/python-mode'
NeoBundle 'vim-airline/vim-airline'
NeoBundle 'vim-airline/vim-airline-themes'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'vim-pandoc/vim-pandoc'
NeoBundle 'vim-pandoc/vim-pandoc-syntax'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'scrooloose/nerdtree.git'
NeoBundle 'ervandew/supertab'
NeoBundle 'xolox/vim-misc'
NeoBundle 'xolox/vim-easytags'
NeoBundle 'rhysd/vim-clang-format'
NeoBundle 'rust-lang/rust.vim'
NeoBundle 'chase/vim-ansible-yaml'
NeoBundle 'fatih/molokai'
NeoBundle 'Valloric/YouCompleteMe'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'easymotion/vim-easymotion'
NeoBundle 'dracula/vim'
NeoBundle 'dkprice/vim-easygrep'

"Navigation
" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" Installation check.
if iCanHazNeoBundle == 0
    echo "Installing Bundles, please ignore key map error messages"
    echo ""
    :NeoBundleInstall
endif

NeoBundleCheck

syntax on

autocmd FileType unite call s:unite_settings()

function! s:unite_settings()
  let b:SuperTabDisabled=1
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
  imap <silent><buffer><expr> <C-x> unite#do_action('split')
  imap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
  nmap <buffer> <ESC> <Plug>(unite_exit)
endfunction

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
let g:unite_source_history_yank_enable = 1
if executable('pt')
  let g:unite_source_grep_command = 'pt'
  let g:unite_source_grep_default_opts = '--nogroup --nocolor '
  let g:unite_source_grep_recursive_opt = ''
  let g:unite_source_grep_encoding = 'utf-8'
  let g:unite_source_rec_async_command = 'pt --nocolor -g=. '
endif

nnoremap <silent> <C-p> :Unite -start-insert -buffer-name=files -winheight=10 file_rec/async<cr>
nnoremap <space>/ :Unite grep:. -buffer-name=search-buffer<CR>
nnoremap <leader>y :<C-u>Unite -buffer-name=yank history/yank<CR>

set paste

set expandtab shiftwidth=4 softtabstop=4 tabstop=4
autocmd FileType make set noexpandtab shiftwidth=8 softtabstop=0
autocmd FileType yaml retab

set nolist
set listchars=tab:▹\ ,trail:▿

"eol:↵,
set keymap=russian-jcukenwin
set iminsert=0
set imsearch=0
highlight lCursor guifg=NONE guibg=Cyan
set wildmode=longest,list,full
set wildmenu
nmap <leader>l :set list! relativenumber!<CR>
nmap <leader>p :set list! number!<CR>

set incsearch hlsearch smartcase
set path=.,,**
set gdefault
set showmatch
set modeline nowrap
set encoding=utf8
set termencoding=utf8
set laststatus=2
set number
set cursorline
set hidden
set autowrite

let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_user_command = 'pt --nocolor -g=. %s'
let g:ctrlp_working_path_mode = 'ra'
let g:SuperTabDefaultCompletionType = "<c-x><c-o>"

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

let g:airline_symbols.linenr = '␤ '
let g:airline_symbols.paste = 'ρ'
let g:airline_left_sep = '»'
let g:airline_right_sep = '«'
let g:airline#extensions#syntastic#enabled = 0

let g:pymode_lint_cwindow = 0
let g:pymode_folding = 0
let g:pymode_lint_checker = "pylint,pep8,pyflakes"
let g:pymode_lint_minheight = 10
let g:pymode_lint_maxheight = 12
let g:pymode_breakpoint = 0
let g:pymode_rope = 0

let g:go_disable_autoinstall = 1
let g:go_bin_path = expand("~/go/bin")
let g:go_fmt_command = "goimports"
au FileType go nmap <Leader>t <Plug>(go-test)
au FileType go nmap <Leader>ds <Plug>(go-def-split)
au FileType go nmap <Leader>i <Plug>(go-info)
au FileType go nmap gd <Plug>(go-def)
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <leader>b <Plug>(go-build)
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_interfaces = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_term_enabled = 0

let g:clang_format#code_style = "llvm"
let g:clang_format#auto_format = 0
let g:clang_format#detect_style_file = 1
let g:clang_format#style_options = {
            \ "IndentWidth" : 8,
            \ "UseTab" : "Always",
            \ "AllowShortIfStatementsOnASingleLine" : "false",
            \ "Standard" : "C++11",
            \ "IndentCaseLabels" : "false",
            \ "BreakBeforeBraces" : "Linux"}

set colorcolumn=81
set ttyfast
set wildmenu

" Press space to clear search highlighting
nnoremap <silent> <Space> :noh<CR>
nnoremap <NL> i<CR><ESC>

" Map toggling paste mode
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set invpaste
set mouse=a

nnoremap <leader>p :PyLintWindowToggle<CR>
nnoremap <F3> :cnext<CR>
nnoremap <S-F3> :cprev<CR>
nnoremap s :exec "normal i".nr2char(getchar())."\e"<CR>
nnoremap S :exec "normal a".nr2char(getchar())."\e"<CR>
nnoremap <F5> :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <F6> :set paste<CR>m`O<Esc>``:set nopaste<CR>
nnoremap <F8> :TagbarToggle<CR>

"disable arrows
inoremap  <Up>     <NOP>
inoremap  <Down>   <NOP>
inoremap  <Left>   <NOP>
inoremap  <Right>  <NOP>
noremap   <Up>     <NOP>
noremap   <Down>   <NOP>
noremap   <Left>   <NOP>
noremap   <Right>  <NOP>

autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

ab pdb import pdb; pdb.set_trace()
ab ipdb import ipdb; ipdb.set_trace()
ab shabang #!/usr/bin/python
ab utf8 # coding: utf-8
ab ifmain if __name__ == "__main__":

" Remove trailing spaces
autocmd BufWritePre * %s/\s\+$//e

if has('gui_running')
    color dracula
    set guifont=Terminus\ 10
else
endif

let g:NERDTreeWinPos = "right"

let g:ansible_options = {'ignore_blank_lines': 0}
let g:ansible_options = {'documentation_mapping': '<C-K>'}
colorscheme dracula

" Snippets
let g:UltiSnipsExpandTrigger="<C-e>"
let g:UltiSnipsJumpForwardTrigger="<c-l>"
let g:UltiSnipsJumpBackwardTrigger="<c-h>"
