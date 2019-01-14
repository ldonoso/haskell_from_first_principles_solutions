module Content06 where

--divideThenAdd :: (Num a, Fractional a) => a -> a -> a
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1


data Mood = Blah

instance Show Mood where
    show _ = "Blah"

class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a

newtype Age = Age Integer deriving (Show, Eq)

instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n
    defaultNumber = Age 65

newtype Year = Year Integer deriving (Show, Eq)

instance Numberish Year where
    fromNumber x = Year x
    toNumber (Year x) = x
    defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed 
    where
        a1 = toNumber a
        a'1 = toNumber a'
        summed = a1 + a'1

--------------------------------------------------------------------------------
data Trivial = Trivial

instance Eq Trivial where
    (==) _ _ = True

instance Show Trivial where
    show _ = "Trivial"

data DayOfTheWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Ord)

instance Eq DayOfTheWeek where
    Mon == Mon = True
    Tue == Tue = True
    Wed == Wed = True
    Thu == Thu = True
    Fri == Fri = True
    Sat == Sat = True
    Sun == Sun = True
    _ == _ = False

--------------------------------------------------------------------------------
data Identity a = Identity a

instance Eq a => Eq (Identity a) where
    (Identity v) == (Identity v') = v == v'

--------------------------------------------------------------------------------
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (TisAn x) == (TisAn x') = x == x'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (Two x y) == (Two x' y') = x == x' && y == y'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (TisAnInt x) == (TisAnInt x') = x == x'
    (TisAString x) == (TisAString x') = x == x'
    _ == _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (Pair x y) == (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (Tuple x y) == (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (ThisOne x) == (ThisOne x') = x == x'
    (ThatOne x) == (ThatOne x') = x == x'
    _ == _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (Hello x) == (Hello x') = x == x'
    (Goodbye x) == (Goodbye x') = x == x'
    _ == _ = False

--------------------------------------------------------------------------------
add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
    if x > 6
    then x + y
    else x

check' :: Ord a => a -> a -> Bool
check' a a' = a == a'
