module HTMLOutput (outputHTML) where

import Text.XHtml
import qualified Data.Map as M
import Data.Maybe
import Data.Time

import Types

outputHTML :: (Show k, Show t) =>  M.Map k (Maybe Version, Maybe Version, Maybe Version, Maybe Version, Maybe Version) -> t -> String
outputHTML map time = showHtml $ page time << mkTable map

myTitle = "Haskell Package Version Tracker"

page time content = thehtml << (header << thetitle << myTitle +++
                           body << (
                                h1 << myTitle +++
                                p << ("Last update: " +++ show time) +++
                                content +++
                                footer))

mkTable m = table << (
                tr << (th << "Package" +++
                       th << "Hackage" +++
                       th << "Debian experimental" +++
                       th << "Debian sid" +++
                       th << "Debian jessie" +++
                       th << "Debian wheezy") +++ 
                (concatHtml $ map row $ M.toAscList m)
                )
  where row (pkg,(h,de,du,dj,dw)) = tr << (td << show pkg +++
                                  maybe none mkCell h +++
                                  maybe none mkCell de +++
                                  maybe none mkCell du +++
                                  maybe none mkCell dj +++
                                  maybe none mkCell dw)
        mkCell (Version v u) = td << hotlink u << v
        none = td << "–"

footer = p << ("This is created by " +++
               hotlink "http://darcs.nomeata.de/hpvt/" << "hptv" +++
               ", written by " +++
               hotlink "mailto:mail@joachim-breitner.de" << "Joachim Breitner" +++
               ". At the moment it is updated automatically every night.")
