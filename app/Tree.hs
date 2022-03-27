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


getLeavesAt :: Eq k => [k] -> Tree k v -> Maybe [Tree k v]
getLeavesAt [] t = Just [t]
getLeavesAt (k:ks) (Node k' ls)
    | k == k' = case mapMaybe (getLeavesAt ks) ls of
                  [] -> Nothing
                  ts -> Just $ concat ts
getLeavesAt _ _ = Nothing
