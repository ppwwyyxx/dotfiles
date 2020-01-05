#!/bin/bash -e
# File: dump.sh

gconftool-2 --dump /apps/guake > apps-guake.xml

dconf dump /org/gnome/desktop/screensaver/ > gnome-screensaver.ini
