{- app/Extra.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module Extra where

import Data.Bifunctor (Bifunctor(first))
import Data.Text (Text, pack)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft = first

funMaybeToRight :: a -> (b -> Maybe c) -> b -> Either a c
funMaybeToRight d f x = maybeToRight d $ f x

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight l Nothing  = Left l
maybeToRight _ (Just v) = Right v

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy f []     = []
classifyBy f (x:xs) = (x : filter (f x) xs)
                    : classifyBy f (filter (not . f x) xs)

-- | Decompose a list into its head and tail.
--
-- * If the list is empty, returns @('mempty', [])@
-- * If the list is non-empty, returns @(x, xs)@,
-- where @x@ is the head of the list and @xs@ its tail.
uncons :: Monoid a => [a] -> (a,[a])
uncons []     = (mempty,[])
uncons (x:xs) = (x,xs)

seperateOn :: Eq a => a -> [a] -> [[a]]
seperateOn _ [] = []
seperateOn sep list = l : seperateOn sep (drop 1 r)
    where (l,r) = span (/= sep) list

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t e = b >>= \b -> if b then t else e

-- | A total version of @tail . dropWhile (/= x)@
after :: Eq a => a -> [a] -> [a]
after x xs = case dropWhile (/= x) xs of
          []  -> []
          xs' -> tail xs'

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

showT :: Show a => a -> Text
showT = pack . show

prependBool :: Bool -> a -> [a] -> [a]
prependBool b x xs = if b then x:xs else xs

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = take n $ iterate f x

-- | @splitAtExactly 2 "12:00" == ("12","00")@, but @splitAtExactly 2 "1" == ("","1")@
splitAtExactly :: Int -> [a] -> ([a],[a])
splitAtExactly n list = if length l < n then ([], list) else (l,r)
    where (l,r) = splitAt n list

safeHead :: MonadFail m => [a] -> m a
safeHead []    = fail "Extra.safeHead: Head on empty list"
safeHead (x:_) = return x
