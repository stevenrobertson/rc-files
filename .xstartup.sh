#!/bin/zsh
# Post-desktop-environment program launch commands

xrdb .Xdefaults

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "add Mod4 = Caps_Lock"

if [ "`hostname`" = "isis" ]; then
    smuxi-frontend-gnome &!
#    xchat &!
fi

if [ "`hostname`" = "anubis" ]; then
    xmonad &!
fi

mail-notification --sm-disable &!

if which mitter; then
    mitter &!
fi

if which gwibber; then
    gwibber &!
fi

if which pidgin; then
    pidgin &!
fi


