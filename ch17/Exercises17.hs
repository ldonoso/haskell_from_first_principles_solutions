module Exercises17 where

import Control.Applicative
import qualified Data.Monoid as DM
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = fmap fromList arbitrary

instance DM.Monoid (List a) where
    mempty = Nil

    mappend Nil ys = ys
    mappend (Cons x xs) ys = (Cons x (DM.mappend xs ys))

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) fs xs = flatMap (\f -> fmap f xs) fs

instance Eq a => EqProp (List a) where
    (=-=) = eq

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold DM.mappend Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' 0 xs = Nil
take' n Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs in take' 3000 l
              ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c 
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)
zipWith' _ _ _= Nil

zipWithExtending :: (a -> b -> c) -> List a -> List b -> List c 
zipWithExtending f (Cons x Nil) ys = zipWith' f (repeat' x) ys
zipWithExtending f xs (Cons y Nil) = zipWith' f xs (repeat' y)
zipWithExtending f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWithExtending f xs ys)
zipWithExtending _ _ _ = Nil  -- Any other case we have an input Nil

instance Applicative ZipList' where
    pure = ZipList' . pure
    (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ (zipWithExtending ($) fs xs)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
    arbitrary = fmap ZipList' arbitrary

testZipList :: IO ()
testZipList = quickBatch $ applicative (undefined :: ZipList' (Int, Int, Int))

--------------------------------------------------------------------------------
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First a) _ = (First a)
    (<*>) _ (First a) = (First a)
    (<*>) (Second f) (Second b) = Second (f b)

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = frequency [ (1, fmap First arbitrary) , (1, fmap Second arbitrary) ]

data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Error e) = Error e
    fmap f (Success a) = Success (f a)

-- This is different
instance DM.Monoid e => Applicative (Validation e) where
    pure = Success
    (<*>) (Error e) (Error e') = (Error $ DM.mappend e e')
    (<*>) (Error e) _ = (Error e)
    (<*>) _ (Error e) = (Error e)
    (<*>) (Success f) (Success b) = Success (f b)

instance (Eq a, Eq b) => EqProp (Validation a b) where
    (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = frequency [ (1, fmap Error arbitrary) , (1, fmap Success arbitrary) ]

testValidation :: IO ()
testValidation = do
    quickBatch $ applicative (undefined :: (Sum String) (Int, Int, Int))
    quickBatch $ applicative (undefined :: (Validation String) (Int, Int, Int))

--------------------------------------------------------------------------------
-- instance Applicative [] where
--     pure :: a -> [a]
--     (<*>) :: [(a -> b)] -> [a] -> [b]
-- 
-- instance Applicative IO where
--     pure ::  a -> IO a
--     (<*>) :: IO (a -> b) -> IO a -> IO b
-- 
-- instance (DM.Monoid a) => Applicative ((,) a) where
--     pure :: b -> (a, b)
--     (<*>) :: (a, (b -> c)) -> (a, b) -> (a, c)
-- 
-- instance Applicative ((->) e) where
--     pure :: a -> (e -> a)
--     (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

--------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

--------------------------------------------------------------------------------
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

--------------------------------------------------------------------------------
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (DM.Monoid a) => Applicative (Two a) where
    pure b = Two DM.mempty b
    (<*>) (Two a f) (Two a' b) = Two (mappend a a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

--------------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (DM.Monoid a, DM.Monoid b) => Applicative (Three a b) where
    pure b = Three DM.mempty DM.mempty b
    (<*>) (Three a b f) (Three a' b' c) = Three (mappend a a') (mappend b b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

--------------------------------------------------------------------------------
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance (DM.Monoid a) => Applicative (Three' a) where
    pure b = Three' DM.mempty b b
    (<*>) (Three' a f1 f2) (Three' a' b1 b2) = Three' (mappend a a') (f1 b1) (f2 b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq


--------------------------------------------------------------------------------
data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (DM.Monoid a, DM.Monoid b, DM.Monoid c) => Applicative (Four a b c) where
    pure b = Four DM.mempty DM.mempty DM.mempty b
    (<*>) (Four a b c f) (Four a' b' c' d') =
        Four (mappend a a') (mappend b b') (mappend c c') (f d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

--------------------------------------------------------------------------------
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (DM.Monoid a) => Applicative (Four' a) where
    pure b = Four' DM.mempty DM.mempty DM.mempty b
    (<*>) (Four' a1 a2 a3 f) (Four' a1' a2' a3' b) =
        Four' (mappend a1 a1') (mappend a2 a2') (mappend a3 a3') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq


--------------------------------------------------------------------------------
testApplicatives :: IO ()
testApplicatives = do
    quickBatch $ applicative (undefined :: Identity (String, Float, Float))
    quickBatch $ applicative (undefined :: Pair (String, Float, Float))
    quickBatch $ applicative (undefined :: (Two (DM.Product Int)) (String, Int, Integer))
    quickBatch $ applicative (undefined :: (Three (DM.Product Int) (DM.Sum Integer)) (String, Int, Integer))
    quickBatch $ applicative (undefined :: (Three' (DM.Product Int)) (String, Int, Integer))
    quickBatch $ applicative (undefined ::
        (Four (DM.Product Int) (DM.Sum Integer) String) (String, Int, Integer))
    quickBatch $ applicative (undefined ::
        (Four' (DM.Product Int)) (String, Int, Integer))

--------------------------------------------------------------------------------
stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs = liftA3 (,,) as bs cs


words3 = combos stops vowels stops
