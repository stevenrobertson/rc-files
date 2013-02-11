#!/bin/zsh
# Post-desktop-environment program launch commands

. ~/.zshrc

xrdb -override .Xdefaults
[ -f "$HOME/.xmodmap-`hostname`" ] && xmodmap ~/.xmodmap-`hostname`
setxkbmap -option caps:hyper -option compose:rwin

if ! which lsusb || ! (lsusb | grep -q '05ac:024f')
  setxkbmap -option altwin:swap_lalt_lwin
