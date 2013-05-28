import Data.Maybe
import Data.List
import qualified Data.Map as M
import System.Time
import Control.Applicative ((<$>))

import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity

import Hackage as H
import Debian as D

import HTMLOutput
import CabalDebianMap
import PlatformOutput
import Types
import Utils


main = do 
	putStrLn "Haskell Package Version Tracker"

	hackage <- H.readFile "00-index.tar.gz"
	putStrLn $ "Read " ++ show (M.size hackage) ++ " packages from Hackage"

	debian_unstable <- D.readFile "Sources.unstable" "sid" hackage
	putStrLn $ "Read " ++ show (M.size debian_unstable) ++ " packages from Debian (Unstable)"

	debian_squeeze <- D.readFile "Sources.squeeze" "squeeze" hackage
	putStrLn $ "Read " ++ show (M.size debian_squeeze) ++ " packages from Debian (Squeeze)"

	debian_wheezy <- D.readFile "Sources.wheezy" "wheezy" hackage
	putStrLn $ "Read " ++ show (M.size debian_wheezy) ++ " packages from Debian (Wheezy)"

	debian_experimental <- D.readFile "Sources.experimental" "experimental" hackage
	putStrLn $ "Read " ++ show (M.size debian_experimental) ++ " packages from Debian (Experimental)"

        platform_2010_1_0_0 <- flattenPackageDescription <$> readPackageDescription normal "haskell-platform-2010.1.0.0.cabal"
        platform_2012_2_0_0 <- flattenPackageDescription <$> readPackageDescription normal "haskell-platform-2012.2.0.0.cabal"
        platform_2013_2_0_0 <- flattenPackageDescription <$> readPackageDescription normal "haskell-platform-2013.2.0.0.cabal"
        platform_darcs <- flattenPackageDescription <$> readPackageDescription normal "haskell-platform-darcs.cabal"
        putStrLn $ "Read platform package descriptions"
	
	let combined = stopCombine $ startCombine (,,,,)
                                     `mapCombine` hackage
                                     `mapCombine` debian_experimental
                                     `mapCombine` debian_unstable
                                     `mapCombine` debian_wheezy
                                     `mapCombine` debian_squeeze
	putStrLn $ "Found " ++ show (M.size combined) ++ " total packages"

	time <- getClockTime
	writeFile "output.html" $ outputHTML combined time
	writeFile "cabalDebianMap.txt" $ outputCabalDebianMap combined
        writeFile "platform.html" $ outputPlatform time hackage [
                (platform_2010_1_0_0, [
                    ("Squeeze", debian_squeeze)
                ])
                , (platform_2012_2_0_0, [
                    ("Wheezy", debian_wheezy)
                ])
                , (platform_2013_2_0_0, [
                    ("Unstable", debian_unstable),
                    ("Experimental", debian_experimental)
                ])
                , (platform_darcs, [
                ])
            ]



