#!/bin/bash -e
# File: dump.sh
# Author: Yuxin Wu <ppwwyyxxc@gmail.com>

gconftool-2 --dump /apps/guake > apps-guake.xml

dconf dump /org/gnome/desktop/screensaver/ > gnome-screensaver.ini
