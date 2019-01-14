module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- unfortunate orphan instances. Try to avoid these in code you're going to keep or release.

-- !!TODOLUIS Review this program. Look at the compositions!

instance Monoid a => Monoid (ZipList a) where
    mempty = pure mempty
    mappend = liftA2 mappend

-- Already defined
-- instance Arbitrary a => Arbitrary (ZipList a) where
--     arbitrary = ZipList <$> arbitrary

-- Already defined
-- instance Arbitrary a => Arbitrary (Sum a) where
--     arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where
    (=-=) = eq
