#!/bin/bash
# File: rm_git.sh
# Date: Mon Nov 12 19:09:42 2012 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
rm .vimrc .vim -r
cp ~/.vimrc ~/.vim ./ -rv
rm .zshrc .aliasrc .zsh -r
cp ~/.zshrc ~/.aliasrc ~/.zsh ./ -rv
mv `find .vim |grep 'git$' |tr '\n' ' '` ~/.Trash/ --backup=numbered -f
mv `find .vim |grep '\.gitignore$' |tr '\n' ' '` ~/.Trash/ --backup=numbered -f
