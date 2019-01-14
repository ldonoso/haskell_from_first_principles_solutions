module Exercises04 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

awesome = ["Papuchon", "curry", "Haskell"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Reading syntax
x = (+)

f1 :: [a] -> Int
f1 xs = w `x` 1
    where w = length xs


id = \x -> x

my_first = \(x:xs) -> x

f3 :: (a, b) -> a
f3 (a, b) = a
