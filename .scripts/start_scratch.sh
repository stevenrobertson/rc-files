#!/bin/sh
if tmux has -t scratch; then
    exec tmux -2 attach -d -t scratch
else
    exec tmux -2 new -s scratch
fi
