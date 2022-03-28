{- app/Tree.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE DeriveDataTypeable #-}

module Tree where

import Data.Generics (Data)
import Data.Maybe (mapMaybe)

data Tree a b = Node a [Tree a b] | Leaf b
    deriving (Show, Data)


getLeavesAt :: (MonadFail m, Eq k) => [k] -> Tree k v -> m [Tree k v]
getLeavesAt [] t = return [t]
getLeavesAt (k:ks) (Node k' ls)
    | k == k' && null ks = return ls
    | k == k' = case mapMaybe (getLeavesAt ks) ls of
                  [] -> fail "The path does not exists in the tree"
                  ts -> return $ concat ts
getLeavesAt _ _ = fail "The path does not exists in the tree"

-- | Filters the tree to only the branches where the keys point to
--
-- @filterAt [0,1] (Node 0 [Node 1 [], Node 2 []]) = Just (Node 0 [Node 1 []])@
filterAt :: (MonadFail m, Eq k) => [k] -> Tree k v -> m (Tree k v)
filterAt keys tree = getLeavesAt keys tree >>= buildNode keys
  where
    buildNode [] trees = fail "The path does not exists in the tree"
    buildNode [k] trees = return $ Node k trees
    buildNode (k:ks) trees = do
        tree <- buildNode ks trees
        return $ Node k [tree]
