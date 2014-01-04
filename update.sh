#!/bin/bash
# File: update.sh
# Date: Sat Jan 04 11:22:31 2014 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ ! "$HOST" -eq "KeepMoving" ]]; then
	echo "Not at KeepMoving! "
	exit 1
fi
rm .zshrc .aliasrc .bashrc .zsh -rfv
rm .config/awesome -rfv
cp ~/.zshrc ~/.aliasrc ~/.zsh ~/.bashrc ./ -rvH
cp ~/.config/awesome ./.config -rvH

