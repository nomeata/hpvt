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

module Hackage where 

import Types
import Utils
import Index

import Distribution.PackageDescription
import qualified Data.ByteString.Lazy as LBS

readFile :: FilePath -> IO (DistroInfo)
readFile file = do
        indexFile <- LBS.readFile file
        let index = readIndex indexFile
            hackageVersions = map extract index
        return $ fromListMax hackageVersions
           
extract (p, v, i) = (fromHackage p, Version v url)
  where url = "http://hackage.haskell.org/package/" ++ p
