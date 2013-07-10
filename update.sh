#!/bin/bash
# File: update.sh
# Date: Wed Jul 10 14:55:34 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
if [[ ! "$HOST" -eq "KeepMoving" ]]; then
	echo "Not at KeepMoving! "
	exit 1
fi
rm .zshrc .aliasrc .zsh -rfv
cp ~/.zshrc ~/.aliasrc ~/.zsh ./ -rvH

