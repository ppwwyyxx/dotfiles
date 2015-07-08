#!/bin/bash
# File: update.sh
# Date: Wed Jul 01 19:58:58 2015 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ ! "$HOST" -eq "KeepMoving" ]]; then
	echo "Not at KeepMoving! "
	exit 1
fi
rm .zshrc .bashrc .zsh -rfv
rm .config/awesome -rfv
rm .config/python
cp ~/.zshrc ~/.zsh ~/.bashrc ./ -rvH
cp ~/.config/awesome ./.config -rvH
cp ~/.config/python ./.config -rvH
cp ~/.gitconfig ./ -rvf
cp ~/.xbindkeysrc ./ -fv
cp ~/.cvimrc ./

