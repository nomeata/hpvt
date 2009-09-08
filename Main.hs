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

	debian <- D.readFile "Sources" hackage
	putStrLn $ "Read " ++ show (M.size debian) ++ " packages from Debian"

	
	let combined = mapCombine hackage debian 
	putStrLn $ "Found " ++ show (M.size combined) ++ " total packages"

	time <- getClockTime
	writeFile "output.html" $ outputHTML combined time
	writeFile "cabalDebianMap.txt" $ outputCabalDebianMap combined



