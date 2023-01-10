#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi
export BROWSER=xdg-open
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
# https://github.com/kovidgoyal/kitty/issues/469#issuecomment-763314654
export GLFW_IM_MODULE=ibus
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_STYLE_OVERRIDE=breeze
export XMODIFIERS=@im=fcitx
#export LC_CTYPE=zh_CN.UTF-8	 # for emacs+fcitx

# Gnome keyring black magic
# dbus-update-activation-environment --systemd DISPLAY
# https://github.com/microsoft/vscode/issues/92972#issuecomment-602919353
which gnome-keyring-daemon && eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

[ -x /etc/X11/xinit/xinitrc.d/50-systemd-user.sh ] && . /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

# highdpi: https://doc.qt.io/qt-5/highdpi.html
export QT_AUTO_SCREEN_SCALE_FACTOR=1
export QT_ENABLE_HIGHDPI_SCALING=1
# export QT_SCALE_FACTOR=1.5
# export GDK_SCALE=1.5
export DEEPIN_WINE_SCALE=1.5
# gsettings set org.gnome.desktop.interface scaling-factor 2

which xrdb && xrdb ~/.Xresources
which xmodmap && xmodmap ~/.Xmodmap
which xbindkeys && xbindkeys
which xset && xset -b
which xset && xset r rate 200 40

#export $(dbus-launch)

# to work with slim login-cmd
# [[ -z "$1" ]] && exit

# Uncomment if using awesome
# mkdir -p ~/.cache/awesome
# exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
