module Main where

import Control.Monad (forever) -- [1]
import Data.Char (toLower) -- [2]
import Data.Maybe (isJust) -- [3]
import Data.List (intersperse) -- [4]
import System.Exit (exitSuccess) -- [5]
import System.Random (randomRIO) -- [6]
import Test.Hspec

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 7

gameWords :: IO WordList
gameWords = do
    -- !! Note how you can use pattern matching on a let exp
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w =
                        let l = length w
                        in minWordLength <= l && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    let lengthWordList = length wl
    randomIndex <- randomRIO (0, lengthWordList - 1)
    return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] deriving Eq

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
            (intersperse ' ' $ fmap renderPuzzleChar discovered)
            ++ " Guessed so far: " ++ guessed


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (fmap (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

nFailedAttempts :: Puzzle -> Int
nFailedAttempts (Puzzle word _ guessed) = length $ filter (\c -> not $ elem c word) guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
    where
        zipper c wordChar guessChar = if c == wordChar then Just c else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess , alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling in the word accordingly"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            return (fillInCharacter puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word _ guessed) =
    if nFailedAttempts puzzle >= 7 then
        do
            putStrLn "You Lose!"
            putStrLn ("The word was " ++ word)
            exitSuccess

    else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar then
        do
            putStrLn "You win!"
            exitSuccess
    else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStrLn $ "N failed attempts is: " ++ (show $ nFailedAttempts puzzle)
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
    randomWord <- randomWord'
    let puzzle = freshPuzzle (fmap toLower randomWord)
    runGame puzzle


runTestHangman :: IO ()
runTestHangman = hspec $ do
    describe "Test fillInCharacter" $ do
        it "Guessed letter" $ do
            let puzzle = freshPuzzle "luis"
            let (Puzzle _ filled _) = fillInCharacter puzzle 'l'
            filled `shouldBe` [Just 'l', Nothing, Nothing, Nothing]

        it "Failed letter" $ do
            let puzzle = freshPuzzle "luis"
            let (Puzzle _ filled _) = fillInCharacter puzzle 'x'
            filled `shouldBe` [Nothing, Nothing, Nothing, Nothing]

    describe "Test handleGuess" $ do
        it "Already guessed" $ do
            let puzzle = freshPuzzle "luis"
            puzzle2 <- handleGuess puzzle 'x'
            puzzle3 <- handleGuess puzzle2 'x'
            puzzle3 `shouldBe` puzzle2

        it "Present" $ do
            let puzzle = freshPuzzle "luis"
            puzzle2 <- handleGuess puzzle 'x'
            puzzle3 <- handleGuess puzzle2 'l'
            puzzle3 `shouldBe`
                Puzzle "luis" [Just 'l', Nothing, Nothing, Nothing] "lx"

        it "Missing" $ do
            let puzzle = freshPuzzle "luis"
            puzzle2 <- handleGuess puzzle 'x'
            puzzle3 <- handleGuess puzzle2 'y'
            puzzle3 `shouldBe`
                Puzzle "luis" [Nothing, Nothing, Nothing, Nothing] "yx"
