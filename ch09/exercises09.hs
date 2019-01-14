module Exercises09 where

import Data.Bool
import Data.Char

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myTuples = [(x, y) | x <- mySqr, y <- myCube]
myTuples' = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

lenMyTuples' = length myTuples'

--------------------------------------------------------------------------------
myList = [x^y | x <- [1..5], y <- [2, undefined]]

--------------------------------------------------------------------------------
foldBool :: (Num a, Eq a) => [a] -> [a]
foldBool = fmap (\x -> bool x (-x) (x == 3))

--------------------------------------------------------------------------------
myFilter = filter (\x -> (mod x 3) == 0)

--------------------------------------------------------------------------------
myFilter2 = filter (\x -> not $ elem x ["the", "a", "an"]) . words

--------------------------------------------------------------------------------
myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith (,)

--------------------------------------------------------------------------------
myToUpper :: String -> String
myToUpper = filter isUpper 

myCapitalize :: String -> String
myCapitalize [] = []
myCapitalize (x:xs) = toUpper x : xs

myCapitalize2 :: String -> String
myCapitalize2 [] = []
myCapitalize2 (x:xs) = toUpper x : myCapitalize2 xs

capitalizeFirst :: String -> Char
capitalizeFirst xs = toUpper $ head xs 

capitalizeFirst2 :: String -> Char
capitalizeFirst2 xs = toUpper . head $ xs

capitalizeFirst3 :: String -> Char
capitalizeFirst3 = toUpper . head

--------------------------------------------------------------------------------
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else (myOr xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . fmap f

myElem :: Eq a => a -> [a] -> Bool
myElem x ys = myAny (== x) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish ([]:xs) = squish xs
squish ((y:ys):xs) = y : squish (ys : xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . fmap f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : []) = x
myMaximumBy f (x : x' : xs) =
    case f x x' of
    LT -> myMaximumBy f (x': xs)
    EQ -> myMaximumBy f (x : xs)
    GT -> myMaximumBy f (x : xs)
    
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : []) = x
myMinimumBy f (x : x' : xs) =
    case f x x' of
    LT -> myMinimumBy f (x : xs)
    EQ -> myMinimumBy f (x : xs)
    GT -> myMinimumBy f (x' : xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
    
myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
