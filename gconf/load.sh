#!/bin/bash -e
# File: load.sh

gconftool-2 --load apps-guake.xml

dconf load /org/gnome/desktop/screensaver/ < gnome-screensaver.ini
