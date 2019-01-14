module Cipher where

import Data.Char

ord' :: Char -> Int
ord' = (subtract . ord $ 'a') . ord

chr' :: Int -> Char
chr' = chr . (\x -> x + (ord 'a'))

cipherOne :: Int -> Char -> Char
cipherOne shift = chr' . (flip mod $ 26) . (+ shift) . ord'

caesar :: Int -> String -> String
caesar shift = fmap (cipherOne shift) 

unCaesar :: Int -> String -> String
unCaesar shift = fmap (cipherOne (negate shift))

