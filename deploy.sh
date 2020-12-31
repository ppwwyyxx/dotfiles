#!/bin/bash -e
# File: deploy.sh

# avoid mistakenly deploy on laptop
if [[ "$HOST" == Keep* ]]; then
	echo "at $HOST"
	exit 1
fi

rm ~/.zsh -rf
mkdir -p ~/.config
cp .zsh .zshrc .bashrc .gitconfig .profile ~/ -rvf
cp .xbindkeysrc .toprc .xinitrc .Xmodmap .Xresources ~/ -rvf
cp .conkyrc ~/ -rvf
cp .config/compton.conf ~/.config -rvf

cat << EOT >> .tmux.conf
set -g status-bg green
unbind C-q
set -g prefix C-a
bind C-a send-prefix
EOT
cp .tmux.conf ~/ -vf
cp .tmux ~/ -rvf

chmod 755 ~/.zsh ~/.zsh/Completion
