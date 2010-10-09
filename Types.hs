module Types (PackageName, DistroInfo, fromHackage, fromDebian, fromCabal, Version(..)) where

import Data.Char
import qualified Data.Map as M

import qualified Distribution.Package as D

type DistroInfo = M.Map PackageName Version

newtype PackageName = PN String
data Version = Version { version :: String, url :: URL} deriving (Eq)

instance Ord Version where
	v1 `compare` v2 = version v1 `compare` version v2

type URL = String

instance Eq PackageName where
	(PN n1) == (PN n2) = map toLower n1 == map toLower n2

instance Ord PackageName where
	(PN n1) `compare` (PN n2) = map toLower n1 `compare` map toLower n2

instance Show PackageName where
	show (PN n) = n

fromHackage = PN
fromDebian = PN
fromCabal (D.PackageName n) = PN n
