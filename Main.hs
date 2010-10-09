import Data.Maybe
import Data.List
import qualified Data.Map as M
import System.Time

import Hackage as H
import Debian as D

import HTMLOutput
import CabalDebianMap
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

	
	let combined = stopCombine $ startCombine (,,)
                                     `mapCombine` hackage
                                     `mapCombine` debian_unstable
                                     `mapCombine` debian_squeeze
	putStrLn $ "Found " ++ show (M.size combined) ++ " total packages"

	time <- getClockTime
	writeFile "output.html" $ outputHTML combined time
	writeFile "cabalDebianMap.txt" $ outputCabalDebianMap combined



