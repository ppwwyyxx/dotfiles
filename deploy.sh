#!/bin/bash
# File: deploy.sh
# Date: Sun Feb 03 00:12:24 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
rm ~/.vim ~/.zsh -rf
cp .tmux.conf .vim .zsh .vimrc .zshrc .aliasrc ~/ -rvf
