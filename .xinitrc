#!/bin/zsh

HN="$( hostname -s )"

if [ -d /home/steven/.mozilla/firefox/tmpfs.default ]; then
    /home/steven/.scripts/pack_ffox.sh &
fi

xmodmap /.xmodmap-$HN
xset dmps 7200 7200 7200

eval `dbus-launch --sh-syntax`

if [ $HN == 'hera' ]; then
    awesome
    sleep 100d
fi

