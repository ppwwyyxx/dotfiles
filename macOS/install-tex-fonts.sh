#!/bin/bash -e

wget http://mirrors.ctan.org/fonts/amsfonts.zip
unzip amsfonts.zip

# let adobe tools recognize them
cp -rv amsfonts/pfb/* "$HOME/Library/Application Support/Adobe/Fonts"


