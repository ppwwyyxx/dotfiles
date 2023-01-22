#!/bin/bash -e
# tutorials:
# https://develop.kde.org/docs/extend/plasma/kwin/#clients
# https://develop.kde.org/docs/extend/plasma/kwin/api/
#
# For logging, see https://bugs.kde.org/show_bug.cgi?id=445058
# kwin_x11 --replace will show logs in terminal.
plasmapkg2 --type kwinscript -i myscript
plasmapkg2 --type kwinscript -i virtual-desktops-only-on-primary

