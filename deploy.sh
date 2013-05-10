#!/bin/bash
# File: deploy.sh
# Date: Mon May 06 16:24:08 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

if [[ "$HOST" == "KeepMoving" ]]; then
	echo "at Keepmoving"
	exit 1
fi

rm ~/.vim ~/.zsh -rf
cp .vim .zsh .vimrc .zshrc .aliasrc .bashrc ~/ -rvf

cat << EOT >> .tmux.conf
set -g status-bg green
unbind C-q
set -g prefix C-a
bind C-a send-prefix
EOT
cp .tmux.conf ~/ -vf

chmod 755 ~/.zsh ~/.zsh/Completion
