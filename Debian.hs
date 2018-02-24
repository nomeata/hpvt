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


