#!/bin/bash
# File: deploy.sh
# Date: Fri Feb 08 13:42:36 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

if [[ "$HOST" -eq "KeepMoving" ]]; then
	echo "at Keepmoving"
	exit 1
fi

rm ~/.vim ~/.zsh -rf
cp .tmux.conf .vim .zsh .vimrc .zshrc .aliasrc ~/ -rvf
