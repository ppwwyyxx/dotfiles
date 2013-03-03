#!/bin/bash
# File: update.sh
# Date: Fri Mar 01 10:47:40 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ ! "$HOST" -eq "KeepMoving" ]]; then
	echo "Not at KeepMoving! "
	exit 1
fi
rm .vimrc .vim -rfv
cp ~/.vimrc ~/.vim ./ -rvH
rm .zshrc .aliasrc .zsh -rfv
cp ~/.zshrc ~/.aliasrc ~/.zsh ./ -rv
rm `find .vim |egrep 'git$|svn$'`  -rvf
rm `find .vim |egrep '\.gitignore$'` -rvf
rm ../vim.7z
7z a ../vim.7z ~/.vim -l

