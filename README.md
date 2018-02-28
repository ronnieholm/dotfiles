Create symbolic links

    # Windows, PowerShell
    cmd /c "mklink /j E:\Shared\Software\emacs-24.3\.emacs.d E:\git\dotfiles\emacs.d"

    # Linux, Bash
    ln -s ~/git/dotfiles/emacs.d ~/.emacs.d

On Windows, I launch Emacs with this shortcut:

    C:\Windows\System32\cmd.exe /C "set HOME=E:\Shared\Software\emacs-24.3&&E:\Shared\Software\emacs-24.3\bin\runemacs.exe"
