#!/bin/zsh
# Post-desktop-environment program launch commands

. ~/.zshrc

xrdb .Xdefaults
[ -f "$HOME/.xmodmap-`hostname`" ] && xmodmap ~/.xmodmap-`hostname`

setxkbmap -option caps:hyper -option compose:rwin

if [ "`hostname`" = "isis" ]; then
    pack_ffox.sh
    /home/steven/.scripts/lql &!
    #xchat &!
    #pidgin &!
    synergys -n localhost
fi

if [ "`hostname`" = "aten" ]; then
    synergyc -n right isis
fi

if which mitter; then
    mitter &!
fi

if which pino; then
    pino &!
fi

#if which pidgin; then
    #pidgin &!
#fi

if which nvidia-settings; then
    nvidia-settings -l
fi
