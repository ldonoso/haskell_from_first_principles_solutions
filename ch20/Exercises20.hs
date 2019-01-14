module Exercises20 where

import Data.Foldable

data Constant a b = Constant a

instance Foldable (Constant a) where
    foldMap f (Constant a) = mempty

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' a b b') = mappend (f b) (f b')

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' a b b' b'') =
        mappend (f b) $ mappend (f b') (f b'')

-- !!Note the restriction `Monoid (f a)`
filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF fun = foldMap (\a -> if (fun a) then (pure a) else mempty)

