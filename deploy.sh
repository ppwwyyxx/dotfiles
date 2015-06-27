#!/bin/bash
# File: deploy.sh
# Date: Sat Jun 27 22:34:55 2015 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

# avoid mistakenly deploy on laptop
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
