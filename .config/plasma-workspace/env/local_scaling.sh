#!/bin/bash -e

# highdpi: https://doc.qt.io/qt-5/highdpi.html

export QT_AUTO_SCREEN_SCALE_FACTOR=1
export QT_ENABLE_HIGHDPI_SCALING=1

# export QT_SCALE_FACTOR=1.5
# export GDK_SCALE=1.5
export DEEPIN_WINE_SCALE=1.5
# gsettings set org.gnome.desktop.interface scaling-factor 2

