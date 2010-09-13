#!/bin/zsh
# Post-desktop-environment program launch commands

. ~/.zshrc

xrdb .Xdefaults

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Escape"

if [ "`hostname`" = "isis" ]; then
    pack_ffox.sh
    /home/steven/.scripts/lql
    synergys -n localhost
    DISPLAY=:0.1 xmonad &!
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

if which nvidia-settings; then
    nvidia-settings -l
fi
