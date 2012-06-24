#!/bin/sh
if ! tmux has -t scratch; then
    tmux new -s scratch
    tmux set -s -t scratch set-titles off
fi
exec tmux attach -d -t scratch
