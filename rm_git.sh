#!/bin/bash
# File: rm_git.sh
# Date: Sun Dec 23 17:42:19 2012 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
rm .vimrc .vim -r
cp ~/.vimrc ~/.vim ./ -rv
rm .zshrc .aliasrc .zsh -r
cp ~/.zshrc ~/.aliasrc ~/.zsh ./ -rv
/bin/rm `find .vim |egrep 'git$|svn$'`  -rvf
/bin/rm `find .vim |egrep '\.gitignore$'` -rvf
7z a ../vim.7z ~/.vim
