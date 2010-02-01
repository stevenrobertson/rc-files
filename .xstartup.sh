#!/bin/zsh
# Post-desktop-environment program launch commands

xrdb .Xdefaults

if [ "`hostname`" = "isis" ]; then
    $HOME/.scripts/pack_ffox.sh
    xchat &!
fi
compiz --replace --indirect-rendering --loose-binding ccp &!
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

if which tasque; then
    tasque &!
fi


