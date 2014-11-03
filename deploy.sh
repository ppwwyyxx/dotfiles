#!/bin/bash
# File: deploy.sh
# Date: Wed Sep 17 18:39:13 2014 -0700
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

if [[ "$HOST" == "KeepMoving" ]]; then
	echo "at Keepmoving"
	exit 1
fi

rm ~/.vim ~/.zsh -rf
cp .vim .zsh .vimrc .zshrc .aliasrc .bashrc .gitconfig ~/ -rvf

cat << EOT >> .tmux.conf
set -g status-bg green
unbind C-q
set -g prefix C-a
bind C-a send-prefix
EOT
cp .tmux.conf ~/ -vf

chmod 755 ~/.zsh ~/.zsh/Completion
