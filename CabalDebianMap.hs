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

module CabalDebianMap (outputCabalDebianMap) where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Function
import Text.Printf

import Types
import Utils


outputCabalDebianMap :: (Show k) =>  M.Map k (Maybe Version, Maybe Version, Maybe Version, Maybe Version, Maybe Version) ->  String
outputCabalDebianMap = init . unlines . mapMaybe toLine . M.toList
  where toLine (pkg,(h,v1,v2,v3,v4)) =
                if null vs
                then Nothing
                else Just (printf "%s,%s,%s"
                        (show (show pkg))
                        (show (removeEpoch (upstream (version dv))))
                        (show (url dv)))
            where vs = catMaybes [v1,v2,v3,v4]
                  dv = maximum vs
