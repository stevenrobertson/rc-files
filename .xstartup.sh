#!/bin/zsh
# Post-desktop-environment program launch commands

. ~/.zshrc

xrdb .Xdefaults
[ -f "$HOME/.xmodmap-`hostname`" ] && xmodmap ~/.xmodmap-`hostname`
setxkbmap -option caps:hyper -option compose:rwin
