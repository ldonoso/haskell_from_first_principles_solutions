module Content12 where


type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

isAdult :: Person -> Bool
isAdult p = case p of
    Person _ 20 -> True
    otherwise -> False
