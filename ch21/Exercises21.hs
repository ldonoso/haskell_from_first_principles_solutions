module Exercises21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad


--------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> (f x)
    sequenceA (Identity fa) = fmap Identity fa

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

--------------------------------------------------------------------------------
newtype Constant a b = Constant { getConstant :: a } deriving (Show, Eq)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap f _ = mempty

instance Traversable (Constant a) where
    traverse f (Constant a) = pure (Constant a)
    sequenceA (Constant a) = pure (Constant a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where (=-=) = eq

--------------------------------------------------------------------------------
data Optional a = Nada | Yep a deriving (Show, Eq)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep x) = f x

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep x) = Yep <$> (f x)
    sequenceA Nada = pure Nada
    sequenceA (Yep fa) = fmap Yep fa

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = frequency [ (1, pure Nada) , (1, fmap Yep arbitrary) ]

instance (Eq a) => EqProp (Optional a) where (=-=) = eq


--------------------------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = mappend (f x) (foldMap f xs)

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)
    sequenceA Nil = pure Nil
    sequenceA (Cons fx fxs) = Cons <$> fx <*> (sequenceA fxs)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = frequency [ (1, pure Nil) , (3, liftM2 Cons arbitrary arbitrary) ]

instance (Eq a) => EqProp (List a) where (=-=) = eq


--------------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = (Three a b) <$> (f c)
    sequenceA (Three a b fc) = (Three a b) <$> (fc)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq


--------------------------------------------------------------------------------
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
    foldMap f (Three' _ b b') = mappend (f b) (f b')

instance Traversable (Three' a) where
    traverse f (Three' a b b') = (Three' a) <$> (f b) <*> (f b')
    sequenceA (Three' a fb fb') = (Three' a) <$> (fb) <*> (fb')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

--------------------------------------------------------------------------------
data S n a = S (n a) a deriving (Show, Eq)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S na a) = mappend (foldMap f na) (f a)

instance Traversable n => Traversable (S n) where
    traverse f (S na a) = S <$> (traverse f na) <*> f a
    sequenceA (S nfa fa) = S <$> (sequenceA nfa) <*> fa

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq

--------------------------------------------------------------------------------
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node lt a rt) = Node (fmap f lt) (f a) (fmap f rt)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node lt a rt) = mappend (foldMap f lt) $ mappend (f a) (foldMap f rt)

    foldr _ acc Empty = acc
    foldr f acc (Leaf a) = f a acc
    foldr f acc (Node lt a rt) = foldr f (f a (foldr f acc rt)) lt


instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node lt a rt) = Node <$> (traverse f lt) <*> f a <*> (traverse f rt)

    sequenceA Empty = pure Empty
    sequenceA (Leaf fa) = Leaf <$> fa
    sequenceA (Node flt fa frt) = Node <$> (sequenceA flt) <*> fa <*> (sequenceA frt)

arbTree :: (Arbitrary a) => Int -> Gen (Tree a)
arbTree 0 = pure Empty
arbTree 1 = liftM Leaf arbitrary
arbTree n = frequency [
                (1, liftM Leaf arbitrary),
                (2, liftM3 Node (arbTree (n `div` 2)) arbitrary (arbTree (n `div` 2)))
                ]

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = sized arbTree

instance (Eq a) => EqProp (Tree a) where (=-=) = eq

--------------------------------------------------------------------------------
main = do
    quickBatch (traversable (undefined :: [] (Int, Int, [Int])))
    quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
    quickBatch (traversable (undefined :: (Constant Int) (Int, Int, [Int])))
    quickBatch (traversable (undefined :: (Optional) (Int, Int, [Int])))
    quickBatch (traversable (undefined :: (List) (Int, Int, [Int])))
    quickBatch (traversable (undefined :: (Three Int Int) (Int, Int, [Int])))
    quickBatch (traversable (undefined :: (Three' Int) (Int, Int, [Int])))
    quickBatch (traversable (undefined :: (S Optional) (Int, Int, [Int])))
    quickBatch (traversable (undefined :: (Tree) (Int, Int, [Int])))

