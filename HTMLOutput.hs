-- Copyright © 2007-2022 Joachim Breitner
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module HTMLOutput (outputHTML) where

import Text.XHtml
import qualified Data.Map as M
import Data.Maybe
import Data.Time

import Types

outputHTML :: (Show k, Show t) =>  M.Map k (Maybe Version, Maybe Version, Maybe Version, Maybe Version, Maybe Version) -> t -> String
outputHTML map time = showHtml $ page time << mkTable map

myTitle = "Haskell Package Version Tracker"

page time content = thehtml << (header << (
                                   thetitle << myTitle +++
                                   style << "html {font-family: 'Lato', sans-serif;}"
                                   ) +++
                           body << (
                                h1 << myTitle +++
                                p << ("Last update: " +++ show time) +++
                                content +++
                                footer))

mkTable m = table << (
                tr << (th << "Package" +++
                       th << "Hackage" +++
                       th << "experimental" +++
                       th << "sid" +++
                       th << "bookworm (12)" +++
                       th << "bullseye (11)") +++
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
