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

module Types (PackageName, DistroInfo, fromHackage, fromDebian, fromCabal, fromString, Version(..)) where

import Data.Char
import qualified Data.Map as M

import qualified Distribution.Package as D

import Utils

type DistroInfo = M.Map PackageName Version

newtype PackageName = PN String
data Version = Version { version :: String, url :: URL} deriving (Eq)

instance Ord Version where
        v1 `compare` v2 = version v1 `vCmp` version v2

type URL = String

instance Eq PackageName where
        (PN n1) == (PN n2) = map toLower n1 == map toLower n2

instance Ord PackageName where
        (PN n1) `compare` (PN n2) = map toLower n1 `compare` map toLower n2

instance Show PackageName where
        show (PN n) = n

fromHackage = PN
fromDebian = PN
fromCabal = PN . D.unPackageName
fromString = PN
