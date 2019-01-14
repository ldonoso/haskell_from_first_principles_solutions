{-# LANGUAGE ViewPatterns #-}

module MyFunctors where

import Test.QuickCheck
import Test.QuickCheck.Function
import MyMonoids

--------------------------------------------------------------------------------
functorIdentity' :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity' x = fmap id x == x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type FunctorIdentityCheck a = a -> Bool
type FunctorComposeCheck f a = f a -> Fun a a -> Fun a a -> Bool

--------------------------------------------------------------------------------
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pair x y)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

-- TODOLUIS: Fix it. How to create a type alias
-- newtype Pair' a = Pair a
-- 
-- instance Functor Pair' where
--     fmap f (Pair' x y) = Pair' x (f y)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Four' w x y z)

instance Functor (Four' a) where
    fmap f (Four' w x y z) = Four' w x y (f z)


data Sum a b = Sum a b deriving (Show, Eq)

instance Functor (Sum a) where
    fmap f (Sum x y) = Sum x (f y)

--------------------------------------------------------------------------------
testFunctors :: IO ()
testFunctors = do
    quickCheck (functorIdentity' :: FunctorIdentityCheck [Int])
    quickCheck (functorCompose' :: FunctorComposeCheck [] Int)

    quickCheck (functorIdentity' :: FunctorIdentityCheck (Identity Int))
    quickCheck (functorCompose' :: FunctorComposeCheck Identity Int)
 
    quickCheck (functorIdentity' :: FunctorIdentityCheck (Pair Double))
    quickCheck (functorCompose' :: FunctorComposeCheck Pair String)
 
    quickCheck (functorIdentity' :: FunctorIdentityCheck (Two Int String))
    quickCheck (functorCompose' :: FunctorComposeCheck (Two Int) String)
 
    quickCheck (functorIdentity' :: FunctorIdentityCheck (Three Double Int String))
    quickCheck (functorCompose' :: FunctorComposeCheck (Three String String) String)
 
    quickCheck (functorIdentity' :: FunctorIdentityCheck (Three' Double Int))
    quickCheck (functorCompose' :: FunctorComposeCheck (Three' String) String)
 
    quickCheck (functorIdentity' :: FunctorIdentityCheck (Four Double String Integer Int))
    quickCheck (functorCompose' :: FunctorComposeCheck (Four Double String Int) Int)
 
    quickCheck (functorIdentity' :: FunctorIdentityCheck (Four' String Integer))
    quickCheck (functorCompose' :: FunctorComposeCheck (Four' String) Integer)
       
