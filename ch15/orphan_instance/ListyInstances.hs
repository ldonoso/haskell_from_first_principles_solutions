module ListyInstances where

import Data.Monoid
import Listy

newtype MyListy a = MyListy (Listy a) deriving (Eq, Show)

instance Monoid (MyListy a) where
    mempty = MyListy (Listy [])
    mappend (MyListy (Listy l)) (MyListy (Listy l')) = MyListy $ Listy $ mappend l l'
