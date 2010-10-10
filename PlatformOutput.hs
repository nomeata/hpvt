{-# LANGUAGE PatternGuards #-}
module PlatformOutput (outputPlatform) where

import Text.XHtml hiding (version)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Char
import System.Time

import Distribution.Package hiding (PackageName)
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Version (VersionRange(ThisVersion))
import qualified Data.Version as DV
import Data.Version (showVersion, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S, between, eof)

import Types

outputPlatform :: (Show t) => t -> 
 DistroInfo ->
 [ (PackageDescription,[(String, M.Map PackageName Version)]) ] ->
 String
outputPlatform time hackage datas = showHtml $ page time << mkTable hackage datas

myTitle = "Haskell Platform Version Tracker"

page time content = thehtml << (header << thetitle << myTitle +++
			   body << (
			   	h1 << myTitle +++
			   	p << ("Last update: " +++ show time) +++
				content +++
				footer))

mkTable hackage datas = table << (
		tr << (
                    th << "Package name" +++
                    concatHtml (map (\(p,dists) ->
                        th << (display (pkgName (package p)) +++ br +++
                               display (pkgVersion (package p))) +++
                        concatHtml (map (\(name,_) ->
                            th << name
                        ) dists) +++
                        th << (spaceHtml +++ spaceHtml +++ spaceHtml +++ spaceHtml +++ spaceHtml)
                    ) datas) +++
                    th << "Hackage"
                ) +++
		(concatHtml $ map row $ S.toList pkgs)
		)
  where buildDependsMap pd = M.fromList (map fromDep deps)
          where fromDep (Dependency pkg (ThisVersion ver)) = (fromCabal pkg,ver)
                fromDep d = error $ "Unexpected dependency format " ++ display d
                deps = buildDepends pd ++ buildTools bi
                bi = libBuildInfo (fromJust (library pd))
        maps = map (\(p,dists) -> (buildDependsMap p,dists)) datas
        pkgs = S.unions (map (M.keysSet . fst) maps)
        

        row pkg = tr << (
                      td << show pkg +++
                      concatHtml (map (\(pm,dists) ->
                        case M.lookup pkg pm of
                            Just ver -> 
                                td << showVersion ver +++
                                concatHtml (map (\(_,dm) -> case M.lookup pkg dm of
                                    Just dver ->
                                        mkCell dver $
                                            " " +++ vCmp ver dver
                                    Nothing -> none
                                ) dists) +++
                                emptyCell
                            Nothing ->
                                emptyCell +++
                                concatHtml (map (const emptyCell) dists) +++
                                emptyCell
                      ) maps) +++
                      case M.lookup pkg hackage of
                        Just hver -> mkCell hver noHtml
                        Nothing -> none
                  )
        mkCell (Version v u) more = td << (hotlink u << v +++ more)
	none = td << "–"
        emptyCell = td << noHtml

        vCmp ver (Version dver _) = case dver' `compare` ver of
            LT -> "(<)"
            EQ -> "(=)"
            GT -> "(>)"
            where upstream_version = takeWhile (/= '-') dver
                  dver' | Just ver' <- fromDotless upstream_version
                            = ver'
                        | otherwise
                            = parseVersion' upstream_version

fromDotless str =
    if length str == 8 && all isDigit str
    then Just (DV.Version (map read [take 4 str, take 2 (drop 4 str), drop 6 str]) [])
    else Nothing
        

parseVersion' str =
    case readP_to_S (between (return ()) eof parseVersion) str of
        [(v,"")] -> v
        x -> error $ "Could not parse " ++ str ++ ": " ++ show x


footer = p << ("This is created by " +++
	       hotlink "http://darcs.nomeata.de/hpvt/" << "hptv" +++
	       ", written by " +++
	       hotlink "mailto:mail@joachim-breitner.de" << "Joachim Breitner" +++
	       ". At the moment it is updated automatically every night.")
