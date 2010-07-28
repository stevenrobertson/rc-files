#!/bin/zsh
# Post-desktop-environment program launch commands

xrdb .Xdefaults

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "add Mod4 = Caps_Lock"

if [ "`hostname`" = "isis" ]; then
    DISPLAY=:0.1 xmonad &!
    xcompmgr -a &!
    /home/steven/.scripts/lql
#    xchat &!
fi

if [ "`hostname`" = "anubis" ]; then
    xmonad &!
    xcompmgr -a &!
fi

mail-notification --sm-disable &!

if which mitter; then
    mitter &!
fi

if which pino; then
    pino &!
fi

if which pidgin; then
    pidgin &!
fi


