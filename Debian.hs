module Debian where

import Linspire.Debian.Control.ByteString
import qualified Data.ByteString.Char8 as BS

import Types
import Utils
import Data.List
import qualified Data.Map as M

findHaskellPackages hackage (Control sources) = map getData . filter isHaskellPackage $ sources
  where	getData para = (fromDebian cabalName, Version version url)
  	  where	package' = fieldValue "Package" para
  		package = maybe (error "Source stanza without Package field") BS.unpack package'
  		version' = fieldValue "Version" para
  		version = maybe (error "Source stanza without Package field") BS.unpack version'
		cabalName | Just found <- find ((`M.member` hackage).fromHackage) [
					package,
					drop (length "haskell-") package,
			  	        reverse $ drop (length "haskell-") $ reverse package
					]
					= found
		          | otherwise   = package
		url = "http://packages.debian.org/source/sid/" ++ package

  	isHaskellPackage para = maybe False ( ("ghc6" `isSublistOf`) . BS.unpack)
						(fieldValue "Build-Depends" para)


readFile :: FilePath -> DistroInfo -> IO DistroInfo
readFile file hackage = do
	sourceParse <- parseControlFromFile file
	sources <- case sourceParse of
		Left error -> fail (show error)
		Right source -> return source
	return $ M.fromList $ findHaskellPackages hackage sources


