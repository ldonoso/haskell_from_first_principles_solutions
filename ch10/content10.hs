module Content10 where

r = foldr const 0 [1, 2, 3, undefined]
r' = foldr const 0 $ [1, 2, 3, undefined] ++ undefined


myFolds :: (a -> a -> a) -> a -> [a] -> a
myFolds _ acc [] = acc
myFolds f acc (x : xs) = myFolds f (f acc x) xs

--------------------------------------------------------------------------------
fibs = 1 : scanl (+) 1 fibs

fibs20 = take 20 fibs

fibsLessThan100 = takeWhile (< 100) fibs


myFact = 1 : scanl (\acc x -> (acc + 1) * acc) 1 myFact
