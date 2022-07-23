-- :help options
local options = {
  backup = false,                             -- creates a backupfile
  clipboard = "unnamedplus",                  -- allow neovim to access the system clipboard
  completeopt = { "menuone", "noselect" },    -- mostly just for cmp
  conceallevel = 0,                           -- so that `` is visible in markdown files
  fileencoding = "utf-8",                     -- the encoding written to a fileencoding
  hlsearch = true,                            -- highlight all matches on previous search pattern
  ignorecase = true,                          -- ignore case in search patterns
  mouse = "a",                                -- allow the mouse to be used in neovim
  pumheight = 10,                             -- pop up menu height
  showmode = true,                            -- we don't need to see things like -- INSERT -- anymore
  showtabline = 2,                            -- always show tabs
  smartcase = true,                           -- smart case
  smartindent = true,                         -- make indenting smarter again
  splitbelow = true,                          -- force all horizontal splits to go below current window
  splitright = true,                          -- force all vertical splits to go to the right of current window
  swapfile = false,                           -- create a swapfile
  termguicolors = true,                       -- set term gui colors (most terminals support this)  
  timeoutlen = 1000,                          -- time to wait for a mapped sequence to complete (in milliseconds)
  undofile = true,                            -- enable persistent undofile
  updatetime = 300,                           -- faster completion (4000ms default)
  writebackup = false,                        -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
  expandtab = true,                           -- convert tabs to spaces
  shiftwidth = 2,                             -- the number of spaces inserted for each indentation
  tabstop = 2,                                -- insert 2 spaces for a tabs
  cursorline = false,                         -- highlight the current line
  number = true,                              -- set numbered lined
  relativenumber = false,                     -- set relative numbered lines
  numberwidth = 4,                            -- set number column width (default is 4)
  signcolumn = "yes",                         -- always show the sign column, otherwise it would shift the text each time
  wrap = false,                               -- display lines as one long line
  scrolloff = 8,                              -- number of lines to keep above and below cursor
  sidescrolloff = 8,                          -- number of columns to keep to the left and right of cursor
  guifont = "monospace:h17"                   -- the font used in graphical neovim applications
}

vim.opt.shortmess:append "c"                  -- don't show ins-completion-menu messages

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.cmd "set whichwrap+=<,>,[,],h,l"                -- allow specified keys to move that move the cursor left/right to move to previous/next line when cursor in on first/last character in line
vim.cmd [[set iskeyword+=-]]                        -- treat words seperated by dash (-) as one word
