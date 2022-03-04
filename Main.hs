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

import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Time
import Control.Applicative ((<$>))

import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity

import Hackage as H
import Debian as D

import HTMLOutput
import CabalDebianMap
-- import PlatformOutput
import Types
import Utils


main = do
        putStrLn "Haskell Package Version Tracker"

        hackage <- H.readFile "00-index.tar.gz"
        putStrLn $ "Read " ++ show (M.size hackage) ++ " packages from Hackage"

        debian_unstable <- D.readFile "Sources.unstable" "sid" hackage
        putStrLn $ "Read " ++ show (M.size debian_unstable) ++ " packages from Debian (Unstable)"

        -- debian_squeeze <- D.readFile "Sources.squeeze" "squeeze" hackage
        -- putStrLn $ "Read " ++ show (M.size debian_squeeze) ++ " packages from Debian (Squeeze)"

        debian_bullseye <- D.readFile "Sources.bullseye" "bullseye" hackage
        putStrLn $ "Read " ++ show (M.size debian_bullseye) ++ " packages from Debian (bullseye)"

        debian_bookworm <- D.readFile "Sources.bookworm" "bookworm" hackage
        putStrLn $ "Read " ++ show (M.size debian_bookworm) ++ " packages from Debian (bookworm)"

        debian_experimental <- D.readFile "Sources.experimental" "experimental" hackage
        putStrLn $ "Read " ++ show (M.size debian_experimental) ++ " packages from Debian (Experimental)"

        -- platform_2010_1_0_0 <- flattenPackageDescription <$> readGenericPackageDescription normal "haskell-platform-2010.1.0.0.cabal"
        -- platform_2012_2_0_0 <- flattenPackageDescription <$> readGenericPackageDescription normal "haskell-platform-2012.2.0.0.cabal"
        -- platform_2013_2_0_0 <- flattenPackageDescription <$> readGenericPackageDescription normal "haskell-platform-2013.2.0.0.cabal"
        -- platform_darcs <- flattenPackageDescription <$> readGenericPackageDescription normal "haskell-platform-darcs.cabal"
        -- putStrLn "Read platform package descriptions"

        let combined = stopCombine $ startCombine (,,,,)
                                     `mapCombine` hackage
                                     `mapCombine` debian_experimental
                                     `mapCombine` debian_unstable
                                     `mapCombine` debian_bookworm
                                     `mapCombine` debian_bullseye
        putStrLn $ "Found " ++ show (M.size combined) ++ " total packages"

        time <- getCurrentTime
        writeFile "output.html" $ outputHTML combined time
        writeFile "cabalDebianMap.txt" $ outputCabalDebianMap combined

        -- writeFile "platform.html" $ outputPlatform time hackage [
        --         --(platform_2010_1_0_0, [
        --         --    ("Squeeze", debian_squeeze)
        --         --])
        --         (platform_2013_2_0_0, [
        --             ("Bullseye", debian_bullseye),
        --             ("Bookworm", debian_bookworm),
        --             ("Unstable", debian_unstable),
        --             ("Experimental", debian_experimental)
        --         ])
        --         , (platform_darcs, [
        --         ])
        --     ]



