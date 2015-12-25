#!/bin/bash -e
# File: bootstrap.sh
# Author: Yuxin Wu <ppwwyyxx@gmail.com>

cd
mkdir -p configs
cd configs
git clone https://github.com/ppwwyyxx/dotfiles.git
cd dotfiles
./deploy.sh

cd ..
git clone https://github.com/ppwwyyxx/dotvim.git
cd dotvim
./install.sh
