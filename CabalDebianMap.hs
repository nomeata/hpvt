module CabalDebianMap (outputCabalDebianMap) where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Function
import Text.Printf

import Types
import Utils


outputCabalDebianMap :: (Show k) =>  M.Map k (Maybe Version, Maybe Version, Maybe Version, Maybe Version, Maybe Version) ->  String
outputCabalDebianMap = init . unlines . mapMaybe toLine . M.toList
  where toLine (pkg,(h,v1,v2,v3,v4)) =
                if null vs
                then Nothing
                else Just (printf "%s,%s,%s"
                        (show (show pkg))
                        (show (removeEpoch (upstream (version dv))))
                        (show (url dv)))
            where vs = catMaybes [v1,v2,v3,v4]
                  dv = maximum vs
