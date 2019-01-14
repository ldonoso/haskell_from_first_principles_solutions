module Exercises18 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--------------------------------------------------------------------------------
data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

--------------------------------------------------------------------------------
data PhhhbbtttEither b a = Left' a | Right' b deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' a) = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
    pure = Left'
    (<*>) (Right' b) _ = Right' b
    (<*>) _ (Right' b) = Right' b
    (<*>) (Left' f) (Left' a) = Left' (f a)

instance Monad (PhhhbbtttEither b) where
    return = pure
    (>>=) (Right' b) _ = Right' b
    (>>=) (Left' a) f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = frequency [ (1, fmap Left' arbitrary) , (1, fmap Right' arbitrary) ]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

--------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

--------------------------------------------------------------------------------
-- Remember to use the Functor that Monad requires, then see where the chips fall.
data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

concat' :: (List a) -> (List a) -> (List a)
concat' Nil ys = ys
concat' xs Nil = xs
concat' (Cons a xs) ys = Cons a (concat' xs ys)

join :: List (List a) -> List a
join Nil = Nil
join (Cons Nil xs) = join xs
join (Cons xs ys) = concat' xs (join ys)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) fs xs = join $ fmap (\f -> fmap f xs) fs

instance Monad List where
    return = pure
    (>>=) xs f = join $ fmap f xs

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = fmap fromList arbitrary

instance (Eq a) => EqProp (List a) where (=-=) = eq

--------------------------------------------------------------------------------
testMonads :: IO ()
testMonads = do
    quickBatch $ monad (undefined :: (Nope (Int, Int, Int)))
    quickBatch $ monad (undefined :: ((PhhhbbtttEither String) (Int, Int, Int)))
    quickBatch $ monad (undefined :: (Identity (Int, Int, Int)))
    quickBatch $ monad (undefined :: (List (Int, Int, Int)))

--------------------------------------------------------------------------------
j :: Monad m => m (m a) -> m a
j x = x >>= id 

-- Expecting the following behavior:
-- 
--     Prelude> j [[1, 2], [], [3]]
--     [1,2,3]
--     Prelude> j (Just (Just 1))
--     Just 1
--     Prelude> j (Just Nothing)
--     Nothing
--     Prelude> j Nothing
--     Nothing

-- This is fmap
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= \a -> pure (f a)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
    a <- ma
    b <- mb
    return (f a b)
    
-- This is <*>
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = ma >>= \a -> (mf >>= \f -> pure (f a))

-- You’ll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (a : as) f = do
    b <- f a
    bs <- meh as f
    return (b : bs)


-- Hint: reuse "meh"
flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id
