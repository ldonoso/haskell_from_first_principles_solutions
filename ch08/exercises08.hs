module Exercises08 where


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--------------------------------------------------------------------------------
sumAcc :: (Eq a, Num a) => a -> a
sumAcc 0 = 0
sumAcc a = (+ a) . sumAcc $ a - 1

sumAcc' :: (Eq a, Num a) => (a -> a -> a) -> a -> a
sumAcc' f 0 = 0
sumAcc' f a = (f a) . sumAcc $ a - 1

mulRec :: (Integral a) => a -> a -> a
mulRec 0 _ = 0
mulRec _ 0 = 0
mulRec a 1 = a
mulRec a b = a + mulRec a (b - 1)


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

dividedBy' :: Integral a => a -> a -> (a, a)
dividedBy' num denom
    | num < 0 = negateFst $ dividedBy' (negate num) denom
    | denom < 0 = negateFst $ dividedBy' num (negate denom)
    | otherwise = dividedBy num denom
    where negateFst (x, y) = (negate x, y)

data DividedByResult = Result Integer | DividedByZero deriving Show

dividedBy'' :: Integral a => a -> a -> DividedByResult
dividedBy'' n d
    | d == 0 = DividedByZero
    | otherwise = let (q, m ) = dividedBy' n d in
        Result $ toInteger q

--------------------------------------------------------------------------------
mc91 :: Integral a => a -> a
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 . mc91 $ x + 11

