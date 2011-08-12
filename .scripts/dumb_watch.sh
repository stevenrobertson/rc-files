#!/bin/bash

touch /tmp/$$.old
TIME=$1
shift

while sleep $TIME; do
    $@ > /tmp/$$.out 2>&1
    if [ -n "$(diff /tmp/$$.out /tmp/$$.old)" ]; then
        cp /tmp/$$.out /tmp/$$.old
        echo -e "\n\n\n\n\n\n\n\n"
        cat /tmp/$$.out
    fi
done


