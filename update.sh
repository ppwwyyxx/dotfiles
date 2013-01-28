#!/bin/bash
# File: update.sh
# Date: Mon Jan 28 22:22:24 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ "$HOST" == "KeepMoving" ]]; then
	echo "Not at KeepMoving! "
	exit 1
fi
rm .vimrc .vim -r
cp ~/.vimrc ~/.vim ./ -rv
rm .zshrc .aliasrc .zsh -r
cp ~/.zshrc ~/.aliasrc ~/.zsh ./ -rv
/bin/rm `find .vim |egrep 'git$|svn$'`  -rvf
/bin/rm `find .vim |egrep '\.gitignore$'` -rvf
7z a ../vim.7z ~/.vim
