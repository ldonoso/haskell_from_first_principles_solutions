module FizzBuzz where

import Control.Monad
import Control.Monad.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = execState (mapM_ addResult (enumFromThenTo to (to - 1) from)) []

fizzBuzzMain :: IO ()
fizzBuzzMain = mapM_ putStrLn $ fizzbuzzFromTo 1 100
