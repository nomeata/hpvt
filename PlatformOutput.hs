module PlatformOutput (outputPlatform) where

import Text.XHtml hiding (version)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import System.Time

import Distribution.Package hiding (PackageName)
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Version (VersionRange(ThisVersion))
import Data.Version (showVersion)

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
                                    Just dver -> mkCell dver
                                    Nothing -> none
                                ) dists) +++
                                emptyCell
                            Nothing ->
                                emptyCell +++
                                concatHtml (map (const emptyCell) dists) +++
                                emptyCell
                      ) maps) +++
                      case M.lookup pkg hackage of
                        Just hver -> mkCell hver
                        Nothing -> none
                  )
        mkCell (Version v u) = td << hotlink u << v
	none = td << "–"
        emptyCell = td << noHtml

footer = p << ("This is created by " +++
	       hotlink "http://darcs.nomeata.de/hpvt/" << "hptv" +++
	       ", written by " +++
	       hotlink "mailto:mail@joachim-breitner.de" << "Joachim Breitner" +++
	       ". At the moment it is updated automatically every night.")
