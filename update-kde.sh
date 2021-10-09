#!/bin/bash

# https://github.com/shalva97/kde-configuration-files
for i in plasma-org.kde.plasma.desktop-appletsrc \
  kcminputrc kdeglobals kscreenlockerrc ksplashrc kwinrc plasmarc \
  kwinrulesrc kglobalshortcutsrc baloofilerc latte lattedockrc breezerc \
  Kvantum; do
  cp -rv "$HOME/.config/$i" ./.config
done

dir=$(pwd)

AR_NAME=$dir/kde-local-share.tgz
rm $AR_NAME || true

cd $HOME/.local/share
tar czvf $AR_NAME \
  aurorae color-schemes plasma wallpapers icons
