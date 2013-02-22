#!/bin/bash
# File: deploy.sh
# Date: Mon Feb 18 12:13:16 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

if [[ "$HOST" == "KeepMoving" ]]; then
	echo "at Keepmoving"
	exit 1
fi

rm ~/.vim ~/.zsh -rf
cp .tmux.conf .vim .zsh .vimrc .zshrc .aliasrc ~/ -rvf
