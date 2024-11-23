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

if [ -f ~/.config/plasma-workspace/env/env.sh ]; then
  . ~/.config/plasma-workspace/env/env.sh
fi

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx

# Gnome keyring black magic
# dbus-update-activation-environment --systemd DISPLAY
# https://github.com/microsoft/vscode/issues/92972#issuecomment-602919353
which gnome-keyring-daemon && eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

[ -x /etc/X11/xinit/xinitrc.d/50-systemd-user.sh ] && . /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

which xrdb && xrdb ~/.Xresources
which xmodmap && xmodmap ~/.Xmodmap
#which xbindkeys && xbindkeys  # let bin/background/autorun.sh start it
which xset && xset -b
which xset && xset r rate 200 40

#export $(dbus-launch)

# to work with slim login-cmd
# [[ -z "$1" ]] && exit

# Uncomment if using awesome
# mkdir -p ~/.cache/awesome
# exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
