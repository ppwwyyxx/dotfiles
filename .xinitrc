#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

echo $1 > /tmp/session.log

# X-wise env variables
export EDITOR="vim"
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx
[[ -s ~/.config/python/.startup.py ]] && export PYTHONSTARTUP=~/.config/python/.startup.py

xrdb ~/.Xresources
#xrandr --output VGA1 --right-of LVDS1
xbindkeys
xset -b
xset r rate 200 40

DEFAULT_SESSION=awesome

case $1 in
awesome)
	compton --config ~/.compton.conf &
	exec awesome
	;;
*)
	exec $1
	;;
esac
# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
