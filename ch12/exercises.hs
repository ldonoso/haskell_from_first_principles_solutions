module Exercises12 where

import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

isMaybe x = case x of
    Just _ -> True
    otherwise -> False

getContent (Just x) = x

replaceThe :: String -> String
replaceThe = concat . intersperse " " . fmap getContent . filter isMaybe . fmap notThe . words
    where
        isMaybe x = case x of
                        Just _ -> True
                        otherwise -> False

replaceThe' :: String -> String
replaceThe' = concat . intersperse " " . go . words
    where
        go [] = []
        go (x:xs) = case notThe x of
            Nothing -> go xs
            Just x -> x : go xs


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
    where
        go [] counter = counter
        go ("the" : (x : xs) : xs') counter
            | elem x "aeiou" = go xs' (counter + 1)
            | otherwise = go ((x : xs) : xs') counter
        go (x : xs) counter = go xs counter 
            

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel where
    isVowel x = elem x "aeiou"

--------------------------------------------------------------------------------
newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s
    | nVowels > ((fromIntegral . length $ s) - nVowels) = Nothing
    | otherwise = Just (Word' s)
    where nVowels = countVowels s

--------------------------------------------------------------------------------
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0 = Nothing
    | otherwise = Just (go i)
    where go i = if i == 0 then Zero else Succ (go (i - 1))
