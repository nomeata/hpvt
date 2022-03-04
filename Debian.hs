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

{-# LANGUAGE PatternGuards #-}

module Debian where

import Debian.Control.ByteString
import qualified Data.ByteString.Char8 as BS

import Types
import Utils
import Data.List
import qualified Data.Map as M

findHaskellPackages codename hackage (Control sources) = map getData . filter isHaskellPackage $ sources
  where getData para = (fromDebian cabalName, Version version url)
          where package' = fieldValue "Package" para
                package = maybe (error "Source stanza without Package field") BS.unpack package'
                version' = fieldValue "Version" para
                version = maybe (error "Source stanza without Package field") BS.unpack version'
                cabalName | Just found <- find ((`M.member` hackage).fromHackage) [
                                        package,
                                        drop (length "haskell-") package,
                                        filter (/= '-') $ drop (length "haskell-") package,
                                        reverse $ drop (length "haskell-") $ reverse package
                                        ]
                                        = found
                          | otherwise   = mangle package
                url = "http://packages.debian.org/source/" ++ codename ++ "/" ++ package

        isHaskellPackage para = maybe False ( ("ghc" `isSublistOf`) . BS.unpack)
                                                (fieldValue "Build-Depends" para)


mangle :: String -> String
mangle p | "ganeti-" `isPrefixOf` p = "ganeti"
mangle p = p

readFile :: FilePath -> String -> DistroInfo -> IO DistroInfo
readFile file codename hackage = do
        sourceParse <- parseControlFromFile file
        sources <- case sourceParse of
                Left error -> fail (show error)
                Right source -> return source
        return $ M.fromListWith max $ findHaskellPackages codename hackage sources


