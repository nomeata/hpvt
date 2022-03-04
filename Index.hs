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

{-# LANGUAGE ViewPatterns #-}
module Index where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Distribution.Text (simpleParse)
import Distribution.Version (Version)
import Codec.Compression.GZip(decompress)
import Data.ByteString.Lazy.Char8(ByteString,toStrict)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
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
                    let descr = case parseGenericPackageDescriptionMaybe (toStrict content) of
                            Just genDescr -> packageDescription genDescr
                            Nothing -> error $ "Couldn't read cabal file "++show file
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
