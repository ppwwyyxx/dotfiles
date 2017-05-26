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
export QT_STYLE_OVERRIDE=gtk2
export XMODIFIERS=@im=fcitx

xrdb ~/.Xresources
xmodmap ~/.Xmodmap
xrandr --output HDMI1 --scale 1x1 --right-of eDP1
xbindkeys
xset -b
xset r rate 200 40

#export $(dbus-launch)

# to work with slim login-cmd
# [[ -z "$1" ]] && exit

mkdir -p ~/.cache/awesome
exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
