#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

echo $1 > /tmp/session.log

xrdb ~/.Xresources
xmodmap ~/.Xmodmap
xrandr --output VGA1 --right-of LVDS1
xbindkeys
xset -b
xset r rate 200 40
xflux -z 15213

[[ -z "$1" ]] && exit
exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
#exec $1
