#!/bin/bash

set -e

cd $(dirname $0)
wget -q http://hackage.haskell.org/packages/archive/00-index.tar.gz -O 00-index.tar.gz

wget -q http://ftp.debian.org/debian/dists/unstable/main/source/Sources.xz -O - | xzcat > Sources.unstable
wget -q http://ftp.debian.org/debian/dists/bullseye/main/source/Sources.xz -O - | xzcat > Sources.bullseye
wget -q http://ftp.debian.org/debian/dists/bookworm/main/source/Sources.xz -O - | xzcat > Sources.bookworm
wget -q http://ftp.debian.org/debian/dists/experimental/main/source/Sources.xz -O - | xzcat > Sources.experimental

#wget -q http://code.galois.com/darcs/haskell-platform/haskell-platform.cabal -O - > haskell-platform-darcs.cabal
#wget -q http://code.haskell.org/haskell-platform/haskell-platform.cabal -O - > haskell-platform-darcs.cabal
# wget --no-check-certificate -q https://raw.github.com/haskell/haskell-platform/masteR/haskell-platform.cabal -O haskell-platform-darcs.cabal
#wget -q http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform.cabal -O -  > haskell-platform-2010.1.0.0.cabal
#wget -q http://lambda.haskell.org/platform/download/2012.2.0.0/haskell-platform-2012.2.0.0.tar.gz -O - | tar zOxf - haskell-platform-2012.2.0.0/packages/haskell-platform-2012.2.0.0/haskell-platform.cabal > haskell-platform-2012.2.0.0.cabal

./hpvt >/dev/null
mv output.html public_html/hackagevsdebian.html
curl --digest --netrc-file hackage-netrec --show-error --silent -X PUT -H "Content-type: text/csv" --data-binary '@cabalDebianMap.txt' http://hackage.haskell.org/distro/Debian/packages | grep -q Ok
#mv platform.html public_html/platform.html
# mv cabalDebianMap.txt public_html/cabalDebianMap.txt
