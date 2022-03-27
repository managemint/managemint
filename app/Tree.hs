{- app/Tree.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Tree where

import Prelude hiding (lookup)
import Control.Lens ((<&>), Index, IxValue, Ixed, At)
import Control.Lens.At (Ixed(ix), At (at))
import Control.Monad (join)
import Data.Generics (Data)
import Data.Maybe (listToMaybe, mapMaybe)

data Tree a b = Node a [Tree a b] | Leaf b
    deriving (Show, Data)

-- TODO: Check if the functions are correkt and the lens laws are not violated
-- REFACTOR!

type instance Index (Tree k v) = [k]
type instance IxValue (Tree k v) = v

instance (Eq k) => Ixed (Tree k v) where
    ix k f m = case lookup k m of
                 Just v  -> f v <&> \v' -> replace k v' m
                 Nothing -> pure m

instance (Eq k) => At (Tree k v) where
    at k f m = f mv <&> \case
        Nothing -> maybe m (const (delete k m)) mv
        Just v' -> insert k v' m
        where mv = lookup k m

delete :: Eq k => [k] -> Tree k v -> Tree k v
delete (k:ks) (Node k' ls)
    | k == k' = Node k $ mapMaybe (delete' ks) ls
  where
      delete' [] _ = Nothing
      delete' k t = Just $ delete k t
delete _ t = t

-- | Insert a value in a tree at the given path.
-- If the path already exists, updates the value.
-- When the beginning of the path does not point to the root of the tree,
-- leaves the tree unchanged (e.g. the tree is just a leaf)
insert :: Eq k => [k] -> v -> Tree k v -> Tree k v
insert _ _ (Leaf v) = Leaf v
insert [] v (Node k ls) = Node k [Leaf v]
insert (k:ks) v (Node k' ls)
    | k == k'   = Node k $ insert' ks v ls
    | otherwise = Node k' ls
  where
      insert' [] v ls = [Leaf v]
      insert' (k:ks) v ls
        | null ls   = [insert ks v $ Node k []]
        | otherwise = map (insert (k:ks) v) ls

lookup :: Eq k => [k] -> Tree k v -> Maybe v
lookup (k:ks) (Node k' ls)
    | k == k' = listToMaybe $ mapMaybe (lookup ks) ls
lookup [] (Leaf v) = Just v
lookup _ _ = Nothing

-- | Replaces a value in a tree ponted to by the key
-- If the key points to nothing, leaves the tree unchanged
replace :: Eq k => [k] -> a -> Tree k a -> Tree k a
replace (k:ks) v (Node k' ls)
    | k == k' = Node k (map (replace ks v) ls)
replace [] v (Leaf  v') = Leaf v
replace _ _ t = t

getLeavesAt :: Eq k => [k] -> Tree k v -> Maybe [Tree k v]
getLeavesAt [] (Node _ ls)     = Just ls
getLeavesAt (k:ks) (Node k' ls)
    | k == k' = join . listToMaybe $ map (getLeavesAt ks) ls
getLeavesAt _ _ = Nothing
