#!/bin/bash -e
# tutorials:
# https://develop.kde.org/docs/extend/plasma/kwin/#clients
# https://develop.kde.org/docs/extend/plasma/kwin/api/
#
# For logging, see https://bugs.kde.org/show_bug.cgi?id=445058
# kwin_x11 --replace will show logs in terminal.
rm -rvf $HOME/.local/share/kwin/scripts/myscript
rm -rvf $HOME/.local/share/kwin/scripts/virtual-desktops-only-on-primary
kpackagetool6 -t KWin/Script --install myscript
kpackagetool6 -t KWin/Script --install virtual-desktops-only-on-primary

