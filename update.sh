#!/bin/bash
# File: update.sh
# Date: Wed Jul 01 19:58:58 2015 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ ! "$HOST" -eq "KeepLearning" ]]; then
	echo "Not at Laptop!"
	exit 1
fi
rm .tmux .zshrc .bashrc .zsh -rfv
rm .config/awesome -rfv
rm .config/python
cp ~/.tmux.conf ~/.tmux ./ -rvH
cp ~/.zshrc ~/.zsh ~/.bashrc ./ -rvH
cp ~/.config/awesome ./.config -rvH
cp ~/.config/python ./.config -rvH
#cp ~/.gitconfig ./ -rvf
cp ~/.xbindkeysrc ./ -fv
cp ~/.cvimrc ./

cp ~/.conky* ./ -rvH

cp /etc/X11/xorg.conf.d/50-synaptics.conf ./etc/X11/xorg.conf.d/ -rvH
cp ~/.config/gtk-2.0/* ./.config/gtk-2.0/ -fv
cp ~/.config/gtk-3.0/* ./.config/gtk-3.0/ -fv
cp ~/.gtkrc* ./ -fv
cp ~/.config/termite/config ./.config/termite/ -fv
