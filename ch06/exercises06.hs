module ExercisesCh06 where

import Data.List (sort)

x :: Int -> Int
x blah = blah + 20

-- printIt :: IO ()
-- printIt = putStrLn (show x)

--------------------------------------------------------------------------------
data Person = Person Bool

instance Show Person where
    show (Person x) = show x

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving Show

instance Eq Mood where
    Blah == Blah = True
    Woot == Woot = True
    _ == _ = False

settleDown x = if x == Woot
    then Blah
    else x


type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

--------------------------------------------------------------------------------
data Rocks = Rocks String deriving (Eq, Show, Ord)
data Yeah = Yeah Bool deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

-- phew = Papu "chases" True

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

--------------------------------------------------------------------------------
i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
-- sigmund :: a -> a
sigmund x = myX

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
sigmund' x = myX

-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

--------------------------------------------------------------------------------
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y

-- Hint: use some arithmetic operation to combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = (fromInteger x) + (f y)
