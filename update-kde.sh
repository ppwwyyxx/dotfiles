#!/bin/bash

# https://github.com/shalva97/kde-configuration-files
for i in plasma-org.kde.plasma.desktop-appletsrc \
  kcminputrc kdeglobals kscreenlockerrc ksplashrc kwinrc plasmarc \
  kwinrulesrc kglobalshortcutsrc baloofilerc latte lattedockrc; do
  cp -rv "$HOME/.config/$i" ./.config
done
