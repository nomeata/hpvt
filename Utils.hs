module Utils where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

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
upstream str = case elemIndices '-' str of
		[] -> str
		idx -> take (last idx) str
 
