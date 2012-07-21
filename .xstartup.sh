#!/bin/zsh
# Post-desktop-environment program launch commands

. ~/.zshrc

xrdb -override .Xdefaults
[ -f "$HOME/.xmodmap-`hostname`" ] && xmodmap ~/.xmodmap-`hostname`
setxkbmap -option caps:hyper -option compose:rwin
