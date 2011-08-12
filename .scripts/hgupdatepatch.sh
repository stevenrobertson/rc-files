#!/bin/sh
if [ -n "$1" ]; then
    REV="$1"
else
    REV="-2"
fi

hg revert --all -r "$REV" && hg qpush -f && hg qrefresh && hg qpop -f
