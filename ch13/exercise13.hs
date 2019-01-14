module Exercise13 where

import Control.Monad
import System.Exit
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let line2 = filter (not . flip elem (" '")) $ fmap toLower line1
    case (line2 == reverse line2) of
        True -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Nope!"
            exitSuccess

--------------------------------------------------------------------------------
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Introduce name:"
    name <- getLine
    putStrLn "Introduce age:"
    ageStr <- getLine
    let age = (read ageStr) :: Age
    let person = mkPerson name age
    case person of
        Left personInvalid -> do
            putStrLn $ "Error: " ++ (show personInvalid)
        Right person -> do
            putStrLn $ "Yay!" ++ (show person)

    return ()
    
