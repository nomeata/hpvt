module HTMLOutput (outputHTML) where

import Text.XHtml hiding (version)
import qualified Data.Map as M
import Data.Maybe
import System.Time

import Types

outputHTML :: (Show k, Show t) =>  M.Map k (Maybe Version, Maybe Version) -> t -> String
outputHTML map time = showHtml $ page time << mkTable map

myTitle = "Haskell Package Version Tracker"

page time content = thehtml << (header << thetitle << myTitle +++
			   body << (
			   	h1 << myTitle +++
			   	p << ("Last update: " +++ show time) +++
				content +++
				footer))

mkTable m = table << (
		tr << (th << "Package" +++ th << "Hackage" +++ th << "Debian") +++
		(concatHtml $ map row $ M.toAscList m)
		)
  where	row (pkg,(h,d)) = tr << (td << show pkg +++
		                  maybe none mkCell h +++
		                  maybe none mkCell d )
	mkCell (Version v u) = td << hotlink u << v
	none = td << "–"

footer = p << ("This is created by " +++
	       hotlink "http://darcs.nomeata.de/hpvt/" << "hptv" +++
	       ", written by " +++
	       hotlink "mailto:mail@joachim-breitner.de" << "Joachim Breitner" +++
	       ". At the moment it is updated automatically every night.")
