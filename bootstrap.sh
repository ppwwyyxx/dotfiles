#!/bin/bash -e
# File: bootstrap.sh

if [[ $USER == "codespace" ]]; then
    ./deploy.sh
else
    cd
    mkdir -p install
    cd install
    git clone https://github.com/ppwwyyxx/dotfiles.git
    cd dotfiles
    ./deploy.sh

    cd ..
    git clone https://github.com/ppwwyyxx/dotvim.git
    cd dotvim
    ./install.sh
fi
