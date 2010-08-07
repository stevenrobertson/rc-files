#!/bin/zsh
# Post-desktop-environment program launch commands

xrdb .Xdefaults

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "add Mod4 = Caps_Lock"

if [ "`hostname`" = "isis" ]; then
    pack_ffox.sh
    /home/steven/.scripts/lql
#    xchat &!
fi

if [ "`hostname`" = "aten" ]; then
    synergyc -n right isis
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


