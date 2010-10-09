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
