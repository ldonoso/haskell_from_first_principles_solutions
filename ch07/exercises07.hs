module Exercises07 where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where
        (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d
    where
        (xLast, _) = x `divMod` 10
        (xLast2, _) = xLast `divMod` 10
        (_, d) = xLast2 `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of 
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
    | b == True = x
    | b == False = y


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

