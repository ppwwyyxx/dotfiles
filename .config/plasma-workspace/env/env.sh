#!/bin/bash -e


export BROWSER=xdg-open
# https://github.com/kovidgoyal/kitty/issues/469#issuecomment-763314654
export GLFW_IM_MODULE=ibus
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_STYLE_OVERRIDE=breeze
#export LC_CTYPE=zh_CN.UTF-8	 # for emacs+fcitx

# https://fcitx-im.org/wiki/Using_Fcitx_5_on_Wayland
export XMODIFIERS=@im=fcitx
export QT_IM_MODULES="wayland;fcitx;ibus"


# Fix bug https://bbs.archlinux.org/viewtopic.php?id=282638
# export LIBVA_DRIVER_NAME=nvidia

