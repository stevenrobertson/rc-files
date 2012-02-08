#!/bin/zsh
unset SSH_AUTH_SOCK SSH_CLIENT SSH_CONNECTION SSH_TTY STY

HN="$( hostname -s )"

pack_ffox.sh &!
eval "$(dbus-launch --sh-syntax)"
xset dpms 7200 7200 7200
xterm
xmodmap $HOME/.xmodmap-$HN
xfce4-session
