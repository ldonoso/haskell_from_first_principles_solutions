module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

mySplit :: Char -> String -> [String]
mySplit _ [] = []
mySplit c s
    | head s == c = mySplit c (tail s)
    | otherwise = takeWhile (/= c) s : mySplit c (dropWhile (/= c) s)

myLines :: String -> [String]
myLines = mySplit '\n'

-- This is what we want 'myLines sentences' to equal
shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

main :: IO ()
main =
    print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
