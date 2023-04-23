#!/bin/bash -e
# File: deploy.sh
#
# TODO: deploy by link; deploy lesskey, kitty, ipython

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

git clone --depth 1 -- https://github.com/marlonrichert/zsh-snap.git ~/.zsh/snap/zsh-snap

cp .tmux.conf ~/ -vf
cp .tmux ~/ -rvf

chmod 755 ~/.zsh ~/.zsh/Completion

# https://github.com/thestinger/termite#terminfo
# wget https://raw.githubusercontent.com/thestinger/termite/master/termite.terminfo
# tic -x termite.terminfo
