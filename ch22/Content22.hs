{-# LANGUAGE InstanceSigs #-}

module Content22 where

import Control.Applicative
import Data.Char

--------------------------------------------------------------------------------
hurr :: (Num a) => a -> a
hurr = (*2)

durr :: (Num a) => a -> a
durr = (+10)

m :: Num a => a -> a
m = hurr . durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

--------------------------------------------------------------------------------
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    a <- cap
    b <- rev
    return (a, b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= (\ a -> (rev >>= (\ b -> return(a, b))))

--------------------------------------------------------------------------------
newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

--------------------------------------------------------------------------------
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

--------------------------------------------------------------------------------
instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)


instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

--------------------------------------------------------------------------------
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName , dogName :: DogName , address :: Address } deriving (Eq, Show)
data Dog = Dog { dogsName :: DogName , dogsAddress :: Address } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Reader Person Dog
getDogR' = Dog <$> Reader dogName <*> Reader address

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

getDogRM' :: Person -> Dog
getDogRM' = runReader $
        (Reader dogName) >>= (\ name ->
        ((Reader address) >>= \ address -> return (Dog name address))) 
