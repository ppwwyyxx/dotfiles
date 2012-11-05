#!/bin/bash
# File: rm_git.sh
# Date: Mon Nov 05 16:21:12 2012 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
mv `find .vim |grep 'git$' |tr '\n' ' '` ~/.Trash/ --backup=numbered -f
mv `find .vim |grep '\.gitignore$' |tr '\n' ' '` ~/.Trash/ --backup=numbered -f
