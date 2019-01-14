module MyMonoids where

import Data.Semigroup
import Test.QuickCheck (Arbitrary, quickCheck, elements, arbitrary, CoArbitrary, verboseCheck)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = mappend x mempty == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = mappend mempty x == x

type SemigroupAssocCheck a = a -> a -> a -> Bool

--------------------------------------------------------------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

--------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
   (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

--------------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
   (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
    mempty = Two mempty mempty 
    mappend = (<>)

type TwoAssoc = (Two String String) -> (Two String String) -> (Two String String) -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

--------------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
   (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Monoid a, Monoid b, Monoid c, Semigroup a, Semigroup b, Semigroup c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend = (<>)

type ThreeAssoc = (Three String String String) -> (Three String String String) -> (Three String String String) -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)


--------------------------------------------------------------------------------
data Four a b c d = Four a b c d deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Four w x y z)


--------------------------------------------------------------------------------
newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = (BoolConj True)
    _ <> _ = (BoolConj False)

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return (BoolConj x)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

--------------------------------------------------------------------------------
newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = (BoolDisj False)
    _ <> _ = (BoolDisj True)

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = do
        x <- arbitrary
        return (BoolDisj x)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

--------------------------------------------------------------------------------
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd b) <> _ = Snd b
    _ <> (Snd b) = Snd b
    _ <> x = x

instance (Monoid a) => Monoid (Or a b) where
    mempty = Fst mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]

type OrAssoc = (Or Int Int) -> (Or Int Int) -> (Or Int Int) -> Bool

--------------------------------------------------------------------------------
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
    show _ = "Combine"

instance (Semigroup b) => Semigroup (Combine a b) where
    c1 <> c2 = Combine (\ x -> (unCombine c1 $ x) <> (unCombine c2 $ x))

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\x -> mempty)
    mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        x <- arbitrary
        return (Combine x)

semigroupAssocCombine :: (Eq b, Semigroup b) => (Combine a b) -> (Combine a b) -> (Combine a b) -> a -> Bool
semigroupAssocCombine c1 c2 c3 x = unCombine (c1 <> (c2 <> c3)) x == unCombine ((c1 <> c2) <> c3) x

type CombineAssoc = (Combine Int String) -> (Combine Int String) -> (Combine Int String) -> Int -> Bool

monoidLeftIdentityCombine :: (Eq b, Monoid b, Semigroup b) => (Combine a b) -> a -> Bool
monoidLeftIdentityCombine c x = unCombine (mappend c mempty) x == (unCombine c) x

monoidRightIdentityCombine :: (Eq b, Monoid b, Semigroup b) => (Combine a b) -> a -> Bool
monoidRightIdentityCombine c x = unCombine (mappend mempty c) x == (unCombine c) x

--------------------------------------------------------------------------------
newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
    show _ = "Comp"

instance Semigroup (Comp a) where
    (Comp f1) <> (Comp f2) = Comp (f1 . f2)

instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        x <- arbitrary
        return (Comp x)

semigroupAssocComp :: (Eq a) => (Comp a) -> (Comp a) -> (Comp a) -> a -> Bool
semigroupAssocComp c1 c2 c3 x = unComp (c1 <> (c2 <> c3)) x == unComp ((c1 <> c2) <> c3) x

type CompAssoc = (Comp Int) -> (Comp Int) -> (Comp Int) -> Int -> Bool

--------------------------------------------------------------------------------
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failure a1) <> (Failure a2) = Failure (a1 <> a2)
    (Failure a1) <> _ = Failure a1
    _ <> (Failure a2) = Failure a2
    _ <> (Success b) = Success b

instance (Semigroup a) => Monoid (Validation a b) where
    mempty = undefined
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [(Failure a), (Success b)]

--------------------------------------------------------------------------------
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    AccumulateRight x <> AccumulateRight y = AccumulateRight (go x y) where 
        go (Success b1) (Success b2) = Success (b1 <> b2)
        go (Success b1) _ = Success b1
        go _ (Success b2) = Success b2
        go (Failure a1) _ = Failure a1

instance (Semigroup b) => Monoid (AccumulateRight a b) where
    mempty = undefined
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        x <- arbitrary
        return (AccumulateRight x)

type ValidateAccummulateRight = AccumulateRight (Sum Int) (Product Integer)
type ValidateAcummulateRightAssoc = ValidateAccummulateRight -> ValidateAccummulateRight -> ValidateAccummulateRight -> Bool 

--------------------------------------------------------------------------------
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    AccumulateBoth x <> AccumulateBoth y = AccumulateBoth (go x y) where 
        go (Success b1) (Success b2) = Success (b1 <> b2)
        go (Failure a1) (Failure a2) = Failure (a1 <> a2)
        go (Failure a1) _ = Failure a1
        go _ (Failure a2) = Failure a2

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (AccumulateBoth a b) where
    mempty = AccumulateBoth (Success mempty)
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        x <- arbitrary
        return (AccumulateBoth x)

type ValidateAccummulateBoth = AccumulateBoth (Sum Int) (Product Integer)
type ValidateAcummulateBothAssoc = ValidateAccummulateBoth -> ValidateAccummulateBoth -> ValidateAccummulateBoth -> Bool 

--------------------------------------------------------------------------------
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
    mempty = Mem f
        where f s = (mempty, s)

    mappend m1 m2 = Mem f
        where f s1 =
                let (a1, s2) = runMem m1 s1
                    (a2, s3) = runMem m2 s2
                in (a1 <> a2, s3)

--------------------------------------------------------------------------------
main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (monoidLeftIdentity :: (Identity (Sum Int)) -> Bool)
    quickCheck (monoidRightIdentity :: (Identity String) -> Bool)

    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: (Two String String) -> Bool)
    quickCheck (monoidRightIdentity :: (Two (Product Integer) (Sum Int)) -> Bool)

    quickCheck (semigroupAssoc :: SemigroupAssocCheck (Three String String String))
    quickCheck (monoidLeftIdentity :: (Three String String String) -> Bool)
    quickCheck (monoidRightIdentity :: (Three (Product Integer) (Sum Int) (Product Integer)) -> Bool)

    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck (monoidLeftIdentity :: (Or (Product Int) String) -> Bool)
    quickCheck (monoidRightIdentity :: (Or (Product Int) String) -> Bool)

    quickCheck (semigroupAssocCombine :: CombineAssoc)
    quickCheck (monoidLeftIdentityCombine :: (Combine Int String) -> Int -> Bool)
    quickCheck (monoidRightIdentityCombine :: (Combine Int String) -> Int -> Bool)

    quickCheck (semigroupAssocComp :: CompAssoc)

    quickCheck (semigroupAssoc :: SemigroupAssocCheck (Validation (Sum Int) (Product Integer)))
    quickCheck (semigroupAssoc :: ValidateAcummulateRightAssoc)
    quickCheck (semigroupAssoc :: ValidateAcummulateBothAssoc)

--------------------------------------------------------------------------------
f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

mainMem = do
    print $ runMem (f' `mappend` mempty) 0
    print $ runMem (mempty `mappend` f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' `mappend` mempty) 0 == runMem f' 0
    print $ runMem (mempty `mappend` f') 0 == runMem f' 0
