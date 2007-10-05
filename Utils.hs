module Utils where

import qualified Data.Map as M
import Data.List

isSublistOf []   _   = True
isSublistOf what l = contains' l
 where  contains' []   = False
        contains' text = what `isPrefixOf` text || contains' (tail text)

mapCombine :: (Ord k) => M.Map k a -> M.Map k b -> M.Map k (Maybe a, Maybe b)
mapCombine a b = result
  where left = M.map (\v -> (Just v, Nothing)) a
        right = M.map (\v -> (Nothing, Just v)) b
	result = M.unionWith combine left right
	combine (Just a, Nothing) (Nothing, Just b) = (Just a, Just b)
	combine (Just a, Nothing) _                 = (Just a, Nothing)
	combine _                 (Nothing, Just b) = (Nothing, Just b)
	combine _                 _                 = (Nothing, Nothing) -- should not happen

fromListMax :: (Ord k, Ord a) => [(k,a)] -> M.Map k a
fromListMax = foldr insert M.empty
  where insert (k,v) = M.alter (higher v) k
        higher v Nothing   = Just v
	higher v (Just v') = Just (max v v')

