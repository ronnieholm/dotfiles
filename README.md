On Linux

    $ git clone https://github.com/ronnieholm/dotfiles.git
    $ ln -s ~/git/dotfiles/emacs/ ~/.emacs.d
    $ ln -s ~/git/dotfiles/nvim/ ~/.config/nvim
    $ git clone --depth 1 https://github.com/wbthomason/packer.nvim \
      ~/.local/share/nvim/site/pack/packer/start/packer.nvim

Open `dotfiles/nvim/lua/rh/packer.lsp` in nvim, source the file by running the
`:so` command. Then run the `:PackerSync` command a few times until errors are
gone.

On Windows

    $ git clone https://github.com/ronnieholm/dotfiles.git
    $ cmd /c "mklink /j c:\users\rh\downloads\emacs-27.1-x86_64\.emacs.d c:\users\rh\source\repos\dotfiles\emacs"
    $ C:\Windows\System32\cmd.exe /C "set HOME=c:\users\rh\downloads\emacs-27.1-x86_64&&c:\users\rh\downloads\emacs-27.1-x86_64\bin\runemacs.exe"
