#!/bin/bash
# File: deploy.sh
# Date: Mon Apr 13 22:55:54 2015 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

if [[ "$HOST" == "KeepMoving" ]]; then
	echo "at Keepmoving"
	exit 1
fi

rm ~/.zsh -rf
cp .zsh .zshrc .aliasrc .bashrc .gitconfig ~/ -rvf

cat << EOT >> .tmux.conf
set -g status-bg green
unbind C-q
set -g prefix C-a
bind C-a send-prefix
EOT
cp .tmux.conf ~/ -vf

chmod 755 ~/.zsh ~/.zsh/Completion
