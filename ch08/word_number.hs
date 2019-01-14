module WordNumber where
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! n

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse $ go n
    where
        go 0 = []
        go n = let (q, m) = divMod n 10 in m : go q
    
wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
