{-# LANGUAGE ViewPatterns #-}
module Index where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Distribution.Text (simpleParse)
import Distribution.Version (Version)
import Codec.Compression.GZip(decompress)
import Data.ByteString.Lazy.Char8(ByteString,unpack)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Package
import System.FilePath.Posix
import MaybeRead (readPMaybe)
import Data.Maybe

type Index = [(String,String,PackageDescription)]
type IndexMap = Map.Map String (Set.Set Version)

{-
readIndex :: ByteString -> Index
readIndex str = do
    let unziped = decompress str
        untared = Tar.read unziped
    entr <- archiveEntries untared
    case splitDirectories (tarFileName (entryHeader entr)) of
        [".",pkgname,vers,file] -> do
            let descr = case parsePackageDescription (unpack (entryData entr)) of
                    ParseOk _ genDescr -> flattenPackageDescription genDescr
                    _  -> error $ "Couldn't read cabal file "++show file
            return (pkgname,vers,descr)
        _ -> fail "doesn't look like the proper path"
-}

readIndex :: ByteString -> Index
readIndex str =
    catMaybes . Tar.foldEntries extract [] (error.show) $ Tar.read $ decompress str
  where extract entry = (:) $ case Tar.entryContent entry of
         Tar.NormalFile content _ ->
            case splitDirectories' (Tar.entryPath  entry) of
                [pkgname,vers,file] -> do
                    let descr = case parseGenericPackageDescription (unpack content) of
                            ParseOk _ genDescr -> packageDescription genDescr
                            _  -> error $ "Couldn't read cabal file "++show file
                    Just (pkgname,vers,descr)
                _ -> fail $ "doesn't look like the proper path: " ++ Tar.entryPath entry
         _ -> Nothing
        splitDirectories' s = case splitDirectories s of
            ".":ds -> ds
            ds -> ds

searchIndex :: (String -> String -> Bool) -> Index -> [PackageDescription]
searchIndex f ind = map snd $ filter (uncurry f . fst) $ map (\(p,v,d) -> ((p,v),d)) ind

indexMapFromList :: [PackageIdentifier] -> IndexMap
indexMapFromList pids = Map.unionsWith Set.union
    [ Map.singleton name (Set.singleton vers)
    | PackageIdentifier {pkgName = (unPackageName -> name),pkgVersion = vers} <- pids ]

indexToPackageIdentifier :: Index -> [PackageIdentifier]
indexToPackageIdentifier index = do
    (name,vers_str,_) <- index
    Just vers <- return $ simpleParse vers_str
    return $ PackageIdentifier {pkgName = mkPackageName name,pkgVersion = vers}

bestVersions :: IndexMap -> Map.Map String Version
bestVersions = Map.map Set.findMax
