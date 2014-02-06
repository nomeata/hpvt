#!/bin/bash

set -e

cd $(dirname $0)
wget -q http://hackage.haskell.org/packages/archive/00-index.tar.gz -O 00-index.tar.gz

wget -q http://ftp.debian.org/debian/dists/unstable/main/source/Sources.xz -O - | xzcat > Sources.unstable
wget -q http://ftp.debian.org/debian/dists/squeeze/main/source/Sources.bz2 -O - | bunzip2 > Sources.squeeze
wget -q http://ftp.debian.org/debian/dists/wheezy/main/source/Sources.bz2 -O - | bunzip2 > Sources.wheezy
wget -q http://ftp.debian.org/debian/dists/experimental/main/source/Sources.xz -O - | xzcat > Sources.experimental

#wget -q http://code.galois.com/darcs/haskell-platform/haskell-platform.cabal -O - > haskell-platform-darcs.cabal
#wget -q http://code.haskell.org/haskell-platform/haskell-platform.cabal -O - > haskell-platform-darcs.cabal
wget --no-check-certificate -q https://raw.github.com/haskell/haskell-platform/pre-release/haskell-platform.cabal -O haskell-platform-darcs.cabal
#wget -q http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform.cabal -O -  > haskell-platform-2010.1.0.0.cabal
#wget -q http://lambda.haskell.org/platform/download/2012.2.0.0/haskell-platform-2012.2.0.0.tar.gz -O - | tar zOxf - haskell-platform-2012.2.0.0/packages/haskell-platform-2012.2.0.0/haskell-platform.cabal > haskell-platform-2012.2.0.0.cabal

if [ $(hostname) = ravel ]
then
	export GCONV_PATH=/usr/lib/gconv
fi

./hpvt >/dev/null
mv output.html public_html/hackagevsdebian.html
mv platform.html public_html/platform.html
mv cabalDebianMap.txt public_html/cabalDebianMap.txt
