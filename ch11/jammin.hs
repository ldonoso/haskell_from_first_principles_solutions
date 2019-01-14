module Jammin where

import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)
data JamJars = Jam {fruit :: Fruit, jars :: Int} deriving (Eq, Show, Ord)

row1 = Jam {fruit = Plum, jars = 6}
row2 = Jam {fruit = Apple, jars = 6}
row3 = Jam {fruit = Blackberry, jars = 6}
row4 = Jam {fruit = Peach, jars = 3}
row5 = Jam {fruit = Plum, jars = 6}
row6 = Jam {fruit = Peach, jars = 16}
row7 = Jam {fruit = Plum, jars = 16}

allJam = [row1, row2, row3, row4, row5, row6, row7]

getJars :: [JamJars] -> [Int]
getJars = fmap jars

totalJars :: [JamJars] -> Int
totalJars = sum . getJars

mostRow :: [JamJars] -> JamJars
mostRow (x:xs) = foldr (\x y -> if (jars x) > (jars y) then x else y) x xs

compareKind (Jam k _) (Jam k' _) = compare k k'

sortByFruit :: [JamJars] -> [JamJars]
sortByFruit = sortBy compareKind 

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\ x y -> (compareKind x y) == EQ) . sortByFruit
