#!/bin/bash
# File: compile_ycm.sh
# Date: Sat Jul 06 12:07:12 2013 +0800
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>
mkdir ~/ycm_build
cd ~/ycm_build
cmake -G "Unix Makefiles" -DUSE_SYSTEM_LIBCLANG=ON . ~/.vim/bundle/YouCompleteMe/cpp
make ycm_core
rm ~/ycm_build -rf
