module CabalDebianMap (outputCabalDebianMap) where

import qualified Data.Map as M
import Data.Maybe
import Data.List

import Types
import Utils


outputCabalDebianMap :: (Show k) =>  M.Map k (Maybe Version, Maybe Version, Maybe Version) ->  String
outputCabalDebianMap = unlines . mapMaybe toLine . M.toList
  where toLine (pkg,(h,Just dv,_)) = Just (show (show pkg, upstream (version dv), Just (url dv)))
        toLine _                 = Nothing
