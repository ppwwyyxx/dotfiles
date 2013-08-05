#!/bin/bash
# File: update.sh
# Date: Tue Aug 06 00:02:02 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ ! "$HOST" -eq "KeepMoving" ]]; then
	echo "Not at KeepMoving! "
	exit 1
fi
rm .zshrc .aliasrc .bashrc .zsh -rfv
cp ~/.zshrc ~/.aliasrc ~/.zsh ~/.bashrc ./ -rvH

