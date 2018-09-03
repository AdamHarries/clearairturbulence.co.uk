#!/bin/sh
stack build
stack exec site clean
stack exec site build
cp -r _site public
rsync -r public aharries_clearairturbulence@ssh.phx.nearlyfreespeech.net:/home/
rm -rf public
