module Exercises02 where

thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs :: String -> String
rvrs x = drop 9 x ++ " " ++ (take 2 $ drop 6 x) ++ " " ++ take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
