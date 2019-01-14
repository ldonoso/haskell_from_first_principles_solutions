module Content07 where

-- bindExp :: Integer -> String
-- bindExp x = let z = y + x in
--     let y = 5 in "the integer was: "
--     ++ show x ++ " and y was: "
--     ++ show y ++ " and z was: " ++ show z

bindExp :: Integer -> String
bindExp x = let x = 10; y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \x -> x + 1

-- addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5

-- mflip f = \x -> \y -> f y x
mflip f x y = f y x


--------------------------------------------------------------------------------
isItTwo :: Integer -> Bool
isItTwo 3 = False

--------------------------------------------------------------------------------
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username s) (AccountNumber x)) = putStrLn $ s ++ " " ++ show x

--------------------------------------------------------------------------------
data WherePenguinsLive =
    Galapagos
    | Antarctic
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng location) = location

--------------------------------------------------------------------------------
k :: (a, b) -> a
k (x, y) = x

k1 = k ((4-1), 10)

k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c,f))

--------------------------------------------------------------------------------
funzZ x = case x + 1 == 1 of
    True -> "AWesome"
    False -> "Wut"

pal x = case x == reverse(x) of
    True -> "yes"
    False -> "no"

pal' x = case x == y of
    True -> "yes"
    False -> "no"
    where y = reverse(x)
    
--------------------------------------------------------------------------------
-- functionC x y = if (x > y) then x else y
functionC x y = case x > y of
    True -> x
    False -> y

-- ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 n = case even n of
    True -> n + 2
    False -> n

nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

--------------------------------------------------------------------------------
data Employee = Coder | Manager | Veep | CEO deriving (Show, Eq, Ord)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' = case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

--------------------------------------------------------------------------------
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

--------------------------------------------------------------------------------
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    | y <= 0.59 = 'F'
    where y = x / 100


--------------------------------------------------------------------------------
pal'' :: Eq a => [a] -> Bool
pal'' xs
    | xs == reverse xs = True
    | otherwise = False

-- numbers :: (Num a, Ord a) => a -> a
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
