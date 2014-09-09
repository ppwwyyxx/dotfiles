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
export XMODIFIERS="@im=fcitx"
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx

xrandr --output VGA1 --right-of LVDS1
xset -b
xset r rate 170 50

DEFAULT_SESSION=awesome

case $1 in
startxfce4)
	exec dbus-launch startxfce4
	;;
fluxbox)
	exec startfluxbox
	;;
awesome)
	compton --config ~/.compton.conf &
	exec awesome
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
