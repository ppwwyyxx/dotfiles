#!/bin/bash
# File: deploy.sh
# Date: Mon Jan 21 10:32:35 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
rm ~/.vim ~/.zsh
cp .vim .zsh .vimrc .zshrc .aliasrc ~/ -rvf
