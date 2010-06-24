#!/bin/zsh
# Post-desktop-environment program launch commands

xrdb .Xdefaults

xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "add Mod4 = Caps_Lock"

if [ "`hostname`" = "isis" ]; then
    $HOME/.scripts/pack_ffox.sh
    smuxi-frontend-gnome &!
#    xchat &!
fi
#compiz --replace --indirect-rendering --loose-binding ccp &!
#mail-notification --sm-disable &!
#xmonad &!

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


