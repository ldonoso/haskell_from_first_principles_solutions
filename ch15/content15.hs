module Content15 where

import Data.Monoid
import Test.QuickCheck
import Control.Monad

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Monoid a) => Monoid (Optional a) where

    mempty = Nada

    mappend Nada x = x
    mappend x Nada = x
    mappend (Only x) (Only y) = Only (mappend x y)

--------------------------------------------------------------------------------
monoidAssoc :: (Monoid a, Eq a) => a -> a -> a -> Bool
monoidAssoc a b c = (a <> b) <> c == a <> (b <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a
 
--------------------------------------------------------------------------------
data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)

--------------------------------------------------------------------------------
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada

    mappend f@(First' (Only x)) _ = f
    mappend _ f@(First' (Only x)) = f
    mappend _ _ = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary
        elements [First' Nada, First' (Only x)]


firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' Int -> First' Int -> First' Int -> Bool

main' :: IO ()
main' = do
    verboseCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: First' String -> Bool)
    quickCheck (monoidRightIdentity :: First' String -> Bool)

--------------------------------------------------------------------------------
data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)
