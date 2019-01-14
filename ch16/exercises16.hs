{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Exercises16 where

import GHC.Arr

data BoolAndSomethingElse a = False' a | True' a deriving (Show, Eq)

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a) = True' (f a)


data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Show, Eq)

instance Functor BoolAndMaybeSomethingElse where
    fmap f Falsish = Falsish
    fmap f (Truish a) = Truish (f a)

newtype Mu f = InF { outF :: f (Mu f) }

-- TODOLUIS
-- instance (Functor f) => Functor (Mu f) where
--     fmap f (InF outF) = InF (fmap f outF)

-- N.B. We added the type variable `Word` which is not present in the textbook
data D a = D (Array a a) Int Int
--instance Functor D where
--    fmap f (D arr x y) = D (map f arr) x y

--------------------------------------------------------------------------------

data Sum b a = First a | Second b

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

--------------------------------------------------------------------------------

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

data K a b = K a deriving (Show, Eq)

instance Functor (K a) where
    fmap f (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K (f a))

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a) deriving (Show, Eq)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut x) = LiftItOut (fmap f x)

data Parappa f g a = DaWrappa (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Show, Eq)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a xs) = Cons (f a) (fmap f xs)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Show, Eq)

instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print x a) = Print x (f a)
    fmap f (Read f') = Read (f . f')
