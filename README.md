On Linux:

    $ ln -s ~/git/emacs.d ~/.emacs.d

On Windows

    $ git
    C:\Windows\System32\cmd.exe /C "set HOME=c:\users\rh\downloads\emacs-26.3-x86_64&&c:\users\rh\downloads\emacs-26.3-x86_64\bin\runemacs.exe"

Create symbolic links

    # Windows, PowerShell
    $ cmd /c "mklink /j E:\Shared\Software\emacs-24.3\.emacs.d E:\git\emacs.d"

    # Linux, Bash
    $ln -s ~/git/emacs.d ~/.emacs.d

On Windows, I launch Emacs with this shortcut:

    $ cd c:\users\rh\downloads\emacs-26.3-x86_64
    $ git clone https://github.com/ronnieholm/emacs.d.git .emacs.d
    $ C:\Windows\System32\cmd.exe /C "set HOME=c:\users\rh\downloads\emacs-26.3-x86_64&&c:\users\rh\downloads\emacs-26.3-x86_64\bin\runemacs.exe"
