module Phone where

import Data.Char
import Data.List

-- validButtons = "1234567890*#"
type Digit = Char

-- valid presses = [1..4]
type Presses = Int

data DaPhone = DaPhone [(Digit, [Char])] deriving (Show)


phone :: DaPhone
phone = DaPhone [
    ('1', ""), ('2', "abc"), ('3', "def"), ('4', "ghi"), ('5', "jkl"),
    ('6', "mno"), ('7', "pqrs"), ('8', "tuv"), ('9', "wxyz"), ('0', "+ "),
    ('*', "^"), ('#', ".,") ]

getPos :: Eq a => a -> [a] -> Int
getPos a l = go a l 0
    where go a (x:xs) ctr 
            | x == a = ctr
            | otherwise = go a xs (ctr + 1)


getKey :: DaPhone -> Char -> (Digit, Presses)
getKey (DaPhone daPhone) c
    | isDigit c = (\ (d, keys) -> (d, 1 + length keys)) $ head $ filter (\ (d, _) -> c == d) daPhone
    | otherwise = (\ (d, keys) -> (d, 1 + (getPos c keys))) $ head $ filter (\ (_, keys) -> elem c keys) daPhone

convo :: [String]
convo = [
    "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daPhone = concat . fmap (convert daPhone)

convert :: DaPhone -> Char -> [(Digit, Presses)]
convert daPhone c 
    | isUpper(c) = ('*', 1) : (convert daPhone (toLower c))
    | otherwise = [getKey daPhone c]


fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . (fmap snd)

--------------------------------------------------------------------------------
popularest :: (Eq a, Ord a) => [a] -> a
popularest s =
    let 
        sortedList = sortBy (\x y -> compare (length x) (length y)) $ groupBy (==) $ sort s
    in head $ sortedList !! (length sortedList - 1)

reverseTaps :: Char -> [(Digit, Presses)]
reverseTaps = convert phone

costPopularest :: String -> Presses
costPopularest = fingerTaps . reverseTaps . popularest

coolestLtr :: [String] -> Char
coolestLtr = popularest . filter (/= ' ') . concat

coolestWord :: [String] -> String
coolestWord = popularest . concat . fmap words
