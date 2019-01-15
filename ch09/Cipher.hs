module Cipher where

import Data.Char
import Test.QuickCheck

ord' :: Char -> Int
ord' = subtract (ord 'a') . ord

chr' :: Int -> Char
chr' = chr . (+ (ord 'a'))

cipherOne :: Int -> Char -> Char
cipherOne shift = chr' . (flip mod 26) . (+ shift) . ord'

caesar :: Int -> String -> String
caesar shift = fmap (cipherOne shift) 

unCaesar :: Int -> String -> String
unCaesar shift = fmap (cipherOne (negate shift))


encriptV :: Int -> String -> String -> String
encriptV _ [] _ = []
encriptV count (x:xs) cypherW =
    if x == ' '
        then (x : encriptV count xs cypherW)
        else (f : encriptV (count + 1) xs cypherW)
    where f = cipherOne (ord' letter) x
          letter = cypherW !! (count `mod` (length cypherW))

genAlphaStr :: Gen String
genAlphaStr = listOf $ elements ['a'..'z']

test :: IO ()
test = do
    quickCheck (forAll genAlphaStr (\x y -> x == unCaesar y (caesar y x)))
