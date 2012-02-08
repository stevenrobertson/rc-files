#!/bin/bash

# Change this to match your correct profile
STORE="/opt/media/firefox-profile"

if test -z "$(mount | grep -F "${HOME}/.mozilla" )"
then
    mount "${HOME}/.mozilla"
fi
cd "${HOME}/.mozilla"

if test -f ".unpacked"
then
    tar --exclude '.unpacked' --exclude Cache -c . | lzop -c > ${STORE}/packed.tmp.tar.lzo &&\
    mv ${STORE}/packed.tar.lzo      ${STORE}/packed.tar.lzo.old
    mv ${STORE}/packed.tmp.tar.lzo  ${STORE}/packed.tar.lzo
else
    lzop -d -c ${STORE}/packed.tar.lzo | tar x &&\
    touch ".unpacked"
fi

