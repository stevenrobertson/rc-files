#!/bin/zsh

HN="$( hostname -s )"

if [ -d /home/steven/.mozilla/firefox/tmpfs.default ]; then
    /home/steven/.scripts/pack_ffox.sh &
fi

xmodmap $HOME/.xmodmap-$HN
xset dpms 7200 7200 7200

eval "$(dbus-launch --sh-syntax)"

if [ $HN = 'hera' ] || [ $HN = 'hermes' ]; then
    xrandr --output DVI-0 --auto --right-of LVDS
    awesome
    sleep 100d
fi

