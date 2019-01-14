module Content09 where

import Data.Bool

myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo x y 
    | x' == y' = [x]
    | x' > y' = []
    | otherwise = x : myEnumFromTo (succ x) y
    where x' = fromEnum x
          y' = fromEnum y

--------------------------------------------------------------------------------
myWords :: String -> [String]
myWords [] = []
myWords (' ' : xs) = myWords xs
myWords xs = go xs
    where go xs = w : (myWords xs')
                    where w = takeWhile (/= ' ') xs
                          xs' = dropWhile (/= ' ') xs

--------------------------------------------------------------------------------
myLenght :: [a] -> Int
myLenght [] = 0
myLenght (x:xs) = 1 + myLenght xs

