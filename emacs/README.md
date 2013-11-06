Installation

    # Windows, PowerShell
    git clone git@github.com:ronnieholm/emacs.d.git E:\git\emacs.d 

Create symbolic links

    # Windows, PowerShell
    cmd /c "mklink /j E:\Shared\Software\emacs-24.1\.emacs.d E:\git\emacs.d"

    # Linux, Bash
    ln -s ~/git/emacs.d ~/.emacs.d

On Windows, I launch Emacs with this shortcut:

    C:\Windows\System32\cmd.exe /C "set HOME=E:\Shared\Software\emacs-24.1&&E:\Shared\Software\emacs-24.1\bin\runemacs.exe"
