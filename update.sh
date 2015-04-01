#!/bin/bash
# File: update.sh
# Date: Wed Apr 01 19:36:25 2015 +0800
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
cp ~/.gitconfig ./ -rf

