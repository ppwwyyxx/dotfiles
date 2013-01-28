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

export EDITOR="vim"
export XMODIFIERS="@im=ibus"
export QT_MODULE=ibus
export GTK_MODULE=ibus
export PAGER="/usr/bin/less -s"
export BROWSER="$PAGER"
export LESS_TERMCAP_mb=$YELLOW
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

xrandr --output VGA1 --right-of LVDS1
xset -b
xset r rate 170 50

DEFAULT_SESSION=startxfce4

case $1 in
xfce4)
	exec dbus-launch startxfce4
	;;
fluxbox)
	exec startfluxbox
	;;
gnome-shell)
	exec gnome-session
	;;
enlightenment)
	exec enlightenment_start
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
