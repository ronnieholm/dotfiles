Installation

    git clone git@github.com:ronnieholm/DotVim.git $home\vimfiles # Windows, PowerShell
    git clone git@github.com:ronnieholm/DotVim.git ~/.vim         # Linux, Bash

Now cd into the repository and run

    git submodule init
    git submodule update

Create symbolic links

    cmd /c "mklink /h $home\_vimrc $home\vimfiles\vimrc" # Windows, PowerShell
    ln -s ~/.vim/vimrc ~/.vimrc                          # Linux, Bash

Inspired by http://vimcasts.org/episodes/synchronizing-plugins-with-git-submodules-and-pathogen  
