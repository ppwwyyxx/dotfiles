#!/bin/bash -e
# File: deploy.sh
# Date: Fri Jun 09 06:16:47 2017 +0000

# avoid mistakenly deploy on laptop
if [[ "$HOST" == Keep* ]]; then
	echo "at $HOST"
	exit 1
fi

rm ~/.zsh -rf
cp .zsh .zshrc .bashrc .gitconfig .profile ~/ -rvf
cp .xbindkeysrc .toprc .xinitrc .Xmodmap .Xresources ~/ -rvf
cp .compton.conf .conkyrc ~/ -rvf

cat << EOT >> .tmux.conf
set -g status-bg green
unbind C-q
set -g prefix C-a
bind C-a send-prefix
EOT
cp .tmux.conf ~/ -vf
cp .tmux ~/ -rvf

chmod 755 ~/.zsh ~/.zsh/Completion
