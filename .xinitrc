#!/bin/zsh
unset SSH_AUTH_SOCK SSH_CLIENT SSH_CONNECTION SSH_TTY STY

HN="$( hostname -s )"

pack_ffox.sh &!
eval "$(dbus-launch --sh-syntax)"
xset dpms 7200 7200 7200
xmodmap $HOME/.xmodmap-$HN
gnome-settings-daemon &!
./.xstartup.sh
xmonad
sleep 100d
