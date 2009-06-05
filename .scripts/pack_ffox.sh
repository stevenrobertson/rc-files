#!/bin/bash

# Change this to match your correct profile
PROFILE="tmpfs.default"
STORE="/opt/firefox-profile"

cd "${HOME}/.mozilla/firefox"

if test -z "$(mount | grep -F "${HOME}/.mozilla/firefox/${PROFILE}" )"
then
    mount "${HOME}/.mozilla/firefox/${PROFILE}"
fi

if test -f "${PROFILE}/.unpacked"
then
    tar --exclude '.unpacked' --exclude Cache -cpf ${STORE}/packed.tmp.tar "$PROFILE"
    mv ${STORE}/packed.tar      ${STORE}/packed.tar.old
    mv ${STORE}/packed.tmp.tar  ${STORE}/packed.tar
else
    tar xvpf ${STORE}/packed.tar &&\
    touch "${PROFILE}/.unpacked"
fi
