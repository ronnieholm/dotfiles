On Linux

    $ git clone https://github.com/ronnieholm/dotfiles.git
    $ ln -s ~/git/dotfiles/emacs/ ~/.emacs.d

For Emacs to pickup dotnet for language server, run

    $ export DOTNET_ROOT="$(dirname $(which dotnet))"

On Windows

    $ git clone https://github.com/ronnieholm/dotfiles.git
    $ cmd /c "mklink /j c:\users\rh\downloads\emacs-30.1\.emacs.d c:\users\rh\source\repos\dotfiles\emacs"
    $ cmd /C "set HOME=c:\users\rh\downloads\emacs-30.1&&c:\users\rh\downloads\emacs-30.1\bin\runemacs.exe"
