#!/bin/bash
# File: deploy.sh
# Date: Wed Jan 23 23:25:15 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
rm ~/.vim ~/.zsh -rf
cp .vim .zsh .vimrc .zshrc .aliasrc ~/ -rvf
