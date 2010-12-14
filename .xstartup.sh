#!/bin/zsh
# Post-desktop-environment program launch commands

. ~/.zshrc

xrdb .Xdefaults

#xmodmap -e "remove Lock = Caps_Lock"
#xmodmap -e "keysym Caps_Lock = Escape"
xmodmap -e "keycode 66 = Super_L"

if [ "`hostname`" = "isis" ]; then
    pack_ffox.sh
    DISPLAY=:0.1 /home/steven/.scripts/lql
    #synergys -n localhost
    DISPLAY=:0.1 xmonad &!
    DISPLAY=:0.1 xchat &!
    DISPLAY=:0.1 pidgin &!
    DISPLAY=:0.2 xmonad &!
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
