module Exercises14 where

import Test.QuickCheck
import Test.Hspec
import Data.List (sort)
import Data.Char
import Text.Show.Functions


half :: Fractional a => a -> a
half x = x / 2

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = ((*2) . half $ x) == x


runTestHI :: IO ()
runTestHI = quickCheck (prop_halfIdentity :: Double -> Bool)


--------------------------------------------------------------------------------
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go y (x, False) = (x, False)
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)


runTestListOrdered :: IO ()
runTestListOrdered = quickCheck prop
    where
        prop :: [Int] -> Bool
        prop = listOrdered . sort

runTestListOrdered' :: IO ()
runTestListOrdered' = quickCheck prop
    where
        prop = listOrdered [1, 5, 4, 7]

--------------------------------------------------------------------------------
plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

runTestPlusAss :: IO ()
runTestPlusAss = quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)


runTestPlusComm :: IO ()
runTestPlusComm = quickCheck (plusCommutative :: Int -> Int -> Bool)

mulAssociative x y z = x * (y * z) == (x * y) * z
mulCommutative x y = x * y == y * x

runTestMulAss :: IO ()
runTestMulAss = quickCheck (mulAssociative :: Int -> Int -> Int -> Bool)


runTestMulComm :: IO ()
runTestMulComm = quickCheck (mulCommutative :: Int -> Int -> Bool)

genNoZero :: (Eq a, Num a, Arbitrary a) => Gen a
genNoZero = do
    a <- arbitrary
    if a /= 0 then return a else return (-1)

main :: IO ()
main = hspec $ do
    describe "Division tests" $ do
        it "Division Quot" $ do
            property $ forAll genNoZero $ \ x ->
                       forAll genNoZero $ \ y -> (quot x y)*y + (rem x y) == (x :: Integer)
        it "Division Div" $ do
            property $ forAll genNoZero $ \ x ->
                       forAll genNoZero $ \ y -> (div x y)*y + (mod x y) == (x :: Integer)

    describe "Power tests" $ do
        it "Power ass" $ do
            property powerAss

        it "Power comm" $ do
            property powerComm

    describe "List tests" $ do
        it "List reverses" $ do
            property $ \ l -> (reverse . reverse $ l) == id (l :: [Int])

    describe "Function ops" $ do
        it "Dollar sign" $ do
            property $ \ f x -> ((f :: Int -> Int) x) == (f $ x)

        it "Composition" $ do
            property $ \ f g x -> ((f :: Int -> Int) . (g :: Double -> Int) $ x) == (f $ g x)

    describe "foldr" $ do
        it "cons" $ do
            property $ \ l -> (foldr (:) [] (l :: [Int])) == (foldr (\x acc -> [x] ++ acc) [] (l :: [Int]))

        it "++" $ do
            property $ \ l -> (foldr (++) [] (l :: [[Int]])) == concat l

    describe "take" $ do
        it "take" $ do
            property $ \ n xs -> (length $ take (n :: Int) (xs :: [Char])) == n

    describe "roundtrip" $ do
        it "roundtrip" $ do
            property $ \ x -> (read . show $ x) == (x :: Double)


runTestFloats :: IO ()
runTestFloats = hspec $ do
    describe "Float tests" $ do
        it "square" $ do
            property $ \ x -> (square . sqrt $ x) == (x :: Double)

square :: Num a => a -> a
square x = x * x

powerAss :: Int -> Int -> Int -> Bool
powerAss x y z = (x ^ y) ^ z == x ^ (y ^ z)

powerComm :: Int -> Int -> Bool
powerComm x y = x ^ y == y ^ x

--------------------------------------------------------------------------------
twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord w
    | null w = w
    | otherwise = toUpper (head w) : map toLower (tail w)

runTestIdem :: IO ()
runTestIdem = hspec $ do
    it "capitalizeWord" $ do
        property prop_Cap

    it "sort" $ do
        property prop_Sort


prop_Cap :: String -> Bool
prop_Cap x = x' == (twice capitalizeWord x) && x' == (fourTimes capitalizeWord x)
    where x' = capitalizeWord x
    

prop_Sort :: String -> Bool
prop_Sort x = x' == (twice sort x) && x' == (fourTimes sort x)
    where x' = sort x
    
--------------------------------------------------------------------------------

data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFool' :: Gen Fool
genFool' = elements [Fulse, Fulse, Frue]

