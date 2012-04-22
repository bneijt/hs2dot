#!/bin/bash
set -e
REPO="$PWD"
if [[ ! ${REPO} = /tmp* ]]; then
    echo "This script will reset your git repo, so"
    echo "You need to clone your repo in /tmp"
    echo "and run it in that clone"
    echo "e.a. git clone <path to local file>"
    exit 1
fi


mkdir dots
commits="`git log --pretty=format:%H|head -n 50`"
for hash in $commits; do
    git reset --hard "$hash"
    hs2dot
    mv hs2dot.dot dots/"$hash".dot
    dots="$dots ${hash}.dot"
done

cd dots
dotimate $dots
#TODO Make sure all images are of the same size, otherwise ffmpeg will create a partially broken image
ffmpeg -r 15 -i "frames/frame%05d.jpg" movie.webm
