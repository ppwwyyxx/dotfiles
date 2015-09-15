#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

echo $1 > /tmp/session.log

# X-wise env variables
export EDITOR="vim"
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx
[[ -s ~/.config/python/.startup.py ]] && export PYTHONSTARTUP=~/.config/python/.startup.py

xrdb ~/.Xresources
xmodmap ~/.Xmodmap
xrandr --output VGA1 --right-of LVDS1
xbindkeys
xset -b
xset r rate 200 40

[[ -z "$1" ]] && exit
exec $1
