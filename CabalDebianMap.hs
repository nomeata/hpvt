module CabalDebianMap (outputCabalDebianMap) where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Function

import Types
import Utils


outputCabalDebianMap :: (Show k) =>  M.Map k (Maybe Version, Maybe Version, Maybe Version, Maybe Version, Maybe Version) ->  String
outputCabalDebianMap = unlines . mapMaybe toLine . M.toList
  where toLine (pkg,(h,v1,v2,v3,v4)) =
                if null vs
                then Nothing
                else Just (show (show pkg, upstream (version dv), Just (url dv)))
            where vs = catMaybes [v1,v2,v3,v4] 
                  dv = maximumBy (compare `on` version) vs
