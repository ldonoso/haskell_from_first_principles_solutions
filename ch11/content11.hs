{-# LANGUAGE FlexibleInstances #-}

module Content11 where


data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 150)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False
    

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m p) = m

--------------------------------------------------------------------------------
class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42


newtype Goats = Goats Int deriving Show

instance TooMany Goats where
    tooMany (Goats n) = n > 43

instance TooMany (Int, String) where
    tooMany (i, s) = i > 42

instance TooMany (Int, Int) where
    tooMany (i1, i2) = i1 + i2 > 42

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (a1, a2) = tooMany a1 || tooMany a2

--------------------------------------------------------------------------------
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)

--------------------------------------------------------------------------------
data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show
type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show
data Garden' =
    Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener deriving Show

--------------------------------------------------------------------------------

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)
data ProgrammingLanguage = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem , lang :: ProgrammingLanguage } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux , OpenBSDPlusNevermindJustBSDStill , Mac , Windows ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [
    Programmer { os = x, lang = y }
    | x <- allOperatingSystems, y <- allLanguages ]

allProgrammers' :: [Programmer]
allProgrammers' = 
    foldr (\ os l -> (go os) ++ l) [] allOperatingSystems
    where go os = foldr (\ lang l -> (Programmer os lang) : l) [] allLanguages

--------------------------------------------------------------------------------
data Quantum = Yes | No | Both

fq1 :: Quantum -> Bool
fq1 Yes = False
fq1 No = False
fq1 Both = False

fq2 :: Quantum -> Bool
fq2 Yes = False
fq2 No = False
fq2 Both = True

fq3 :: Quantum -> Bool
fq3 Yes = False
fq3 No = True
fq3 Both = False

fq4 :: Quantum -> Bool
fq4 Yes = False
fq4 No = True
fq4 Both = True

fq5 :: Quantum -> Bool
fq5 Yes = True
fq5 No = False
fq5 Both = False

fq6 :: Quantum -> Bool
fq6 Yes = True
fq6 No = False
fq6 Both = True

fq7 :: Quantum -> Bool
fq7 Yes = True
fq7 No = True
fq7 Both = False

fq8 :: Quantum -> Bool
fq8 Yes = True
fq8 No = True
fq8 Both = True
