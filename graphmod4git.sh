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

last_hash=""
if [ ! -d dots ]; then
    mkdir dots
    commits="`git log --pretty=format:%H`"
    for hash in $commits; do
        git reset --hard "$hash"
        graphmod -q --no-cluster Main.hs > dots/"$hash".dot
        echo "${hash}.dot" >> dots/index.list
        if ! cmp "dots/${hash}.dot" "dots/${last_hash}.dot"; then
            echo "${hash}.dot" >> dots/diff.list        
        fi
        last_hash="$hash"
    done
fi
echo "Calling dotimate"
cd dots

if [ ! -d frames ]; then
    dotimate `cat diff.list`
fi

echo "Fixing image sizes"
for image in frames/fram*.jpg;do
    convert "$image" -crop 1152x1152-0-0\! -background white -flatten "$image"
done
echo "Creating movie"
ffmpeg -r 15 -vf "pad=1160:1160" -i "frames/frame%05d.jpg" movie.webm

