module Content20 where

import qualified Data.Foldable as F
import Data.Monoid
import Data.Semigroup


sum :: (F.Foldable t, Num a) => t a -> a
sum t = getSum (F.foldMap Sum t)

product :: (F.Foldable t, Num a) => t a -> a
product t = getProduct (F.foldMap Product t)

elem :: (F.Foldable t, Eq a) => a -> t a -> Bool
elem a t = getAny (F.foldMap (\ x -> Any (x == a)) t)

-- The following is invalidad becase Min has not a monoid
-- Although there is one in Data.Monoid.Ord
-- minimum :: (F.Foldable t, Ord a) => t a -> a
-- minimum t = runMin (F.foldMap Min t)

-- F.foldr deals with Foldable. Prelude's just with []
minimum :: (F.Foldable t, Ord a, Bounded a) => t a -> a
minimum = F.foldr min maxBound

maximum :: (F.Foldable t, Ord a, Bounded a) => t a -> a
maximum = F.foldr max minBound

null :: (F.Foldable t) => t a -> Bool
null = F.foldr (\x acc -> False) True

length :: (F.Foldable t) => t a -> Int
length t = getSum $ F.foldMap (const (Sum 1)) t


toList :: (F.Foldable t) => t a -> [a]
toList = F.foldMap (\x -> [x])

fold :: (F.Foldable t, Monoid m) => t m -> m
fold = F.foldMap id

-- Define foldMap in terms of foldr.
foldMap :: (F.Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f  = F.foldr (\x acc -> (f x) <> acc) mempty
