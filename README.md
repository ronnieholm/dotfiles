On Linux

    $ git clone https://github.com/ronnieholm/dotfiles.git
    $ ln -s ~/git/dotfiles/emacs ~/.emacs.d
    $ ln -s ~/git/dotfiles/nvim ~/.config/nvim

On Windows

    $ git clone https://github.com/ronnieholm/dotfiles.git
    $ cmd /c "mklink /j c:\users\rh\downloads\emacs-27.1-x86_64\.emacs.d c:\users\rh\source\repos\dotfiles\emacs"
    $ C:\Windows\System32\cmd.exe /C "set HOME=c:\users\rh\downloads\emacs-27.1-x86_64&&c:\users\rh\downloads\emacs-27.1-x86_64\bin\runemacs.exe"
