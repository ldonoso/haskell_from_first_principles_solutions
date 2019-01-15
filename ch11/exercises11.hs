module Exercises11 where

import Data.Char
import Data.List (intersperse)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday

f Friday = "Miller Time"

--------------------------------------------------------------------------------
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf l1@(x:xs) l2@(y:ys) =
    ((x == y) && (isSubsequenceOf xs ys)) || isSubsequenceOf l1 ys
    

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = fmap f (words s)
    where f s@(x:xs) = (s, toUpper(x) : xs)

--------------------------------------------------------------------------------
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . fmap (++ ".") . fmap capitalizeWord . paragraphs 

paragraphs :: String -> [String]
paragraphs [] = []
paragraphs s@(x:xs) 
    | x == '.' || x == ' ' = paragraphs xs
    | otherwise = 
        (takeWhile isNotPeriod s) : (paragraphs $ dropWhile isNotPeriod $ s)
        where isNotPeriod x = x /= '.'


