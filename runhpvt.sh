#!/bin/bash

set -e

cd $(dirname $0)
wget -q http://hackage.haskell.org/packages/archive/00-index.tar.gz -O 00-index.tar.gz
wget -q http://ftp.debian.org/debian/dists/unstable/main/source/Sources.bz2 -O - | bunzip2 > Sources
./hpvt >/dev/null
mv output.html public_html/hackagevsdebian.html
