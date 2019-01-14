{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Contents16 where

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where
    fmap _ ItDoesnt = WhatThisIsCalled
    fmap _ WhatThisIsCalled = ItDoesnt
    fmap f (Matter a) = Matter (f a)


changeStruture :: (a -> b) -> (WhoCares a) -> (WhoCares b)
changeStruture _ ItDoesnt = WhatThisIsCalled
changeStruture _ WhatThisIsCalled = ItDoesnt
changeStruture f (Matter a) = Matter (f a)

--------------------------------------------------------------------------------
data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- super NOT okay
instance Functor CountingBad where
    fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)


--------------------------------------------------------------------------------
a = fmap (+1) (read "[1]" :: [Int])
-- 2

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- Just ["Hi,lol","Hellolol"]

c = (*2) . (\x -> x - 2)
-- Prelude> c 1
-- -2

d = ('1' :) . show . (\x -> [x, 1..3])
-- Prelude> d 0
-- "1[0,1,2,3]"

e :: IO Integer
e = let ioi = (readIO "1" :: IO Integer)
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed
-- 3693

e' :: IO Integer
e' = let ioi = (readIO "1" :: IO Integer)
         changed = fmap read $ fmap ("123"++) $ fmap show ioi
    in fmap (*3) changed

--------------------------------------------------------------------------------
liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

--------------------------------------------------------------------------------
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers x) = Yeppers (f x)

--------------------------------------------------------------------------------
newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Show)

instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v

--------------------------------------------------------------------------------
getInt :: IO Int
getInt = fmap read getLine

--------------------------------------------------------------------------------
type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

