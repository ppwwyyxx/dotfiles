#!/bin/bash
# File: rm_git.sh
# Date: Sun Nov 04 09:23:22 2012 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
mv `find .vim |grep 'git$' |tr '\n' ' '` ~/.Trash/ --backup=numbered -f
