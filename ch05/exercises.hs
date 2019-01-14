module Exercises05 where

-- Does it compile?

bigNum = (^) 5
wahoo = bigNum $ 10

x = print
y = print "woohoo!"
z = x "hello world"

a = (+)
b = 5
c = a b 10
d = a c 200

a1 = 12 + b1
b1 = 10000 * c1
c1 = 3

-- functionH :: [a] -> a
functionH (x:_) = x

-- functionC :: (Ord a) => a -> b -> Bool
functionC x y = if (x > y) then True else False

-- functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i x = x

cf :: a -> b -> a
cf x _ = x

c'' :: b -> a -> b
c'' x _ = x

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r (x:xs) = xs
r _ = []

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

af :: (a -> c) -> a -> a
af f x = x

a' :: (a -> b) -> a -> b
a' f x = f x
