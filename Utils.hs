{-# LANGUAGE PatternGuards #-}
module Utils where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Char
import qualified Data.Version as DV
import Data.Version (showVersion, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S, between, eof)

isSublistOf []   _   = True
isSublistOf what l = contains' l
 where  contains' []   = False
        contains' text = what `isPrefixOf` text || contains' (tail text)

type MapDef k v = (v, M.Map k v)

startCombine :: (Ord k) => a -> MapDef k a
startCombine f = (f, M.empty)

mapCombine :: (Ord k) => MapDef k (Maybe b -> c) -> M.Map k b -> MapDef k c
mapCombine (def,a) b = (def Nothing, result)
  where keys = M.keysSet a `S.union` M.keysSet b
        result = fromSet app keys
        app k= M.findWithDefault def k a (M.lookup k b)

stopCombine :: MapDef k c -> M.Map k c
stopCombine = snd

fromSet :: Ord k => (k -> v) -> S.Set k -> M.Map k v
fromSet v = S.fold (\k -> M.insert k (v k)) M.empty


fromListMax :: (Ord k, Ord a) => [(k,a)] -> M.Map k a
fromListMax = foldr insert M.empty
  where insert (k,v) = M.alter (higher v) k
        higher v Nothing   = Just v
	higher v (Just v') = Just (max v v')

upstream :: String -> String
upstream str = case findIndex (`elem` "-+~") str of
		Nothing -> str
		Just idx -> take idx str

removeEpoch :: String -> String
removeEpoch str = case elemIndex ':' str of
                Nothing -> str
                Just idx -> drop (idx+1) str
        
 
vCmp ver1 ver2 = toDVer ver1 `compare` toDVer ver2

toDVer ver | Just ver' <- fromDotless (removeEpoch (upstream ver))
            = ver'
           | otherwise
            = parseVersion' (removeEpoch (upstream ver))


fromDotless str =
    if length str == 8 && all isDigit str
    then Just (DV.Version (map read [take 4 str, take 2 (drop 4 str), drop 6 str]) [])
    else Nothing
        

parseVersion' str =
    case readP_to_S (between (return ()) eof parseVersion) str of
        [(v,"")] -> v
        x -> error $ "Could not parse \"" ++ str ++ "\" as a version: " ++ show x

