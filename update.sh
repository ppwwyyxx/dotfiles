#!/bin/bash
# File: update.sh
# Date: Fri Feb 08 13:42:36 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ ! "$HOST" -eq "KeepMoving" ]]; then
	echo "Not at KeepMoving! "
	exit 1
fi
sudo rm .vimrc .vim -r
cp ~/.vimrc ~/.vim ./ -rv
sudo rm .zshrc .aliasrc .zsh -r
cp ~/.zshrc ~/.aliasrc ~/.zsh ./ -rv
sudo /bin/rm `find .vim |egrep 'git$|svn$'`  -rvf
sudo /bin/rm `find .vim |egrep '\.gitignore$'` -rvf
7z a ../vim.7z ~/.vim
