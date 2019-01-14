module Exercises10 where

import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = (fmap (\ (DbDate x) -> x)) . filter isDbDate
    where
        isDbDate x = case x of
            DbDate _ -> True
            otherwise -> False
        
filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' xs = [x | DbDate x <- xs] 

-- filterDbDate'' :: [DatabaseItem] -> [UTCTime]
-- filterDbDate'' = (fmap (\ (DbDate x) -> x)) 

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (DbNumber x : xs) = x : filterDbNumber xs
filterDbNumber (_ : xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs =
    foldr (\ x y -> if x > y then x else y) (times !! 0) times
    where times = filterDbDate xs

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

sumDb' :: [DatabaseItem] -> Integer
sumDb' = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs =  (fromIntegral $ sumDb xs) / (fromIntegral $ length xs)

--------------------------------------------------------------------------------
stops = "pbtdkg"
vowels = "aeiou"

myWords = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
myWords' = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc' :: String -> Double
seekritFunc' x = (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))

--------------------------------------------------------------------------------
myOr :: [Bool] -> Bool
myOr = foldr (\ x y -> if x then x else y) False

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (foldr (||) False) . (fmap f)

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\x' b -> b || x' == x) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\ x l -> f x : l) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\ x l -> if f x then x : l else l) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\ x l -> f x ++ l) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = foldl go x xs
    where go x y = case (f x y) of
                    GT -> x
                    EQ -> x
                    LT -> y

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = foldl go x xs
    where go x y = case (f x y) of
                    GT -> y
                    EQ -> x
                    LT -> x
