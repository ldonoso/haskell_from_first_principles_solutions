module Addition where

import Test.Hspec
import Test.QuickCheck

-- x = sample arbitrary

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

multipleBy :: (Eq a, Num a) => a -> a -> a
multipleBy _ 0 = 0
multipleBy a b = a + multipleBy a (b - 1)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True

        it "2 + 2 should be equal to 4" $ do
            2 + 2 `shouldBe` 4

        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)

        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)

        it "3 multipled by 0 is 0" $ do
            multipleBy 3 0 `shouldBe` 0

        it "3 multipled by 1 is 3" $ do
            multipleBy 3 1 `shouldBe` 3

        it "3 multipled by 4 is 12" $ do
            multipleBy 3 4 `shouldBe` 12

        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

--------------------------------------------------------------------------------
trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    x <- arbitrary
    y <- arbitrary
    return (x, y)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

-- What QuickCheck actually does so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing) , (3, return (Just a))]

-- frequency :: [(Int, Gen a)] -> Gen a


--------------------------------------------------------------------------------
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
