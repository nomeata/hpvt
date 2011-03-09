#!/bin/bash

set -e

cd $(dirname $0)
wget -q http://hackage.haskell.org/packages/archive/00-index.tar.gz -O 00-index.tar.gz
wget -q http://ftp.debian.org/debian/dists/unstable/main/source/Sources.bz2 -O - | bunzip2 > Sources.unstable
wget -q http://ftp.debian.org/debian/dists/squeeze/main/source/Sources.bz2 -O - | bunzip2 > Sources.squeeze
wget -q http://code.galois.com/darcs/haskell-platform/haskell-platform.cabal -O - > haskell-platform-darcs.cabal
#wget -q http://code.haskell.org/haskell-platform/haskell-platform.cabal -O - > haskell-platform-darcs.cabal
wget -q http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform.cabal -O -  > haskell-platform-2010.1.0.0.cabal
wget -q http://hackage.haskell.org/platform/2010.2.0.0/haskell-platform.cabal -O - > haskell-platform-2010.2.0.0.cabal

./hpvt >/dev/null
mv output.html public_html/hackagevsdebian.html
mv platform.html public_html/platform.html
mv cabalDebianMap.txt public_html/cabalDebianMap.txt
