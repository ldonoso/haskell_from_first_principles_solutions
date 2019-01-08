{-# LANGUAGE InstanceSigs #-}

module Contents23 where

import System.Random
import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Dice = Dice1 | Dice2 | Dice3 | Dice4 | Dice5 | Dice6 deriving (Show, Eq)

intToDice :: Int -> Dice
intToDice n = case n of
    1 -> Dice1
    2 -> Dice2
    3 -> Dice3
    4 -> Dice4
    5 -> Dice5
    6 -> Dice6
    x -> error $ "Invalid int " ++ show n

getDice :: [Dice]
getDice = do
    let s = mkStdGen 1364
        (n1, s1) = randomR (1, 6) s
        (n2, s2) = randomR (1, 6) s1
        (n3, s3) = randomR (1, 6) s2
    fmap intToDice [n1, n2, n3]

rollDie :: State StdGen Dice
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDice n, s)

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= 20 = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> (Int, [Dice])
rollsToGetN n g = go 0 0 g []
    where
        go :: Int -> Int -> StdGen -> [Dice] -> (Int, [Dice])
        go sum count gen dices
            | sum >= n = (count, reverse dices)
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen (intToDice die : dices)

--------------------------------------------------------------------------------
data Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap f (Moi g) = Moi $ \s ->
        let (a, s1) = g s
        in (f a, s1)

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s ->
        let (fa, s1) = f s
            (a, s2) = g s1
        in (fa a, s2)
        
instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ \s0 ->
        let
            (a, s1) = f s0
            Moi f2 = g a
        in f2 s1
