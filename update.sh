#!/bin/bash
# File: update.sh
# Date: Fri May 08 23:12:24 2015 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ ! "$HOST" -eq "KeepMoving" ]]; then
	echo "Not at KeepMoving! "
	exit 1
fi
rm .zshrc .aliasrc .bashrc .zsh -rfv
rm .config/awesome -rfv
rm .config/python
cp ~/.zshrc ~/.aliasrc ~/.zsh ~/.bashrc ./ -rvH
cp ~/.config/awesome ./.config -rvH
cp ~/.config/python ./.config -rvH
cp ~/.gitconfig ./ -rvf
cp ~/.xbindkeysrc ./ -fv

