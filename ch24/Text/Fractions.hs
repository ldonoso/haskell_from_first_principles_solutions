{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Data.Attoparsec.Text (parseOnly)
import Data.String (IsString)

badFraction, alsoBad, shouldWork, shouldAlsoWork :: IsString s => s
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator is 0"
        _ -> return (numerator % denominator)
        

main :: IO ()
main = do
    -- parseOnly is Attoparsec
    print $ parseOnly parseFraction badFraction
    print $ parseOnly parseFraction shouldWork
    print $ parseOnly parseFraction shouldAlsoWork
    print $ parseOnly parseFraction alsoBad

    -- parseString is Trifecta
    print $ parseString parseFraction mempty badFraction
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad


integerEof :: Parser Integer
integerEof = do
    x <- integer
    eof
    return x

parseIntegerMain :: IO ()
parseIntegerMain = do
    print $ parseString integerEof mempty "123"
    print $ parseString integerEof mempty "123abc"
