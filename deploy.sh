#!/bin/bash -e
# File: deploy.sh
# Date: Fri Jun 09 06:16:47 2017 +0000
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

# avoid mistakenly deploy on laptop
if [[ "$HOST" == "KeepMoving" ]]; then
	echo "at Keepmoving"
	exit 1
fi

rm ~/.zsh -rf
cp .zsh .zshrc .bashrc .gitconfig .profile ~/ -rvf
cp .xbindkeysrc .toprc .xinitrc .Xmodmap .Xresources ~/ -rvf

cat << EOT >> .tmux.conf
set -g status-bg green
unbind C-q
set -g prefix C-a
bind C-a send-prefix
EOT
cp .tmux.conf ~/ -vf
cp .tmux ~/ -rvf

chmod 755 ~/.zsh ~/.zsh/Completion
