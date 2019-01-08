{-# LANGUAGE QuasiQuotes #-}

module Exercises24 where

import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ

--------------------------------------------------------------------------------
data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq)

-- Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined by comparing each dot separated identifier from left to right until a difference is found as follows:
-- identifiers consisting of only digits are compared numerically and identifiers with letters or hyphens are compared lexically in ASCII sort order.
-- Numeric identifiers always have lower precedence than non-numeric identifiers.
instance Ord NumberOrString where
    compare (NOSS s1) (NOSS s2) = compare s1 s2
    compare (NOSI i1) (NOSI i2) = compare i1 i2
    compare (NOSI _) (NOSS _) = LT
    compare (NOSS _) (NOSI _) = GT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq, Ord)

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> decimal) <|> (NOSS <$> (some $ notChar '.'))

parseNOSs :: Parser [NumberOrString]
parseNOSs = sepBy parseNOS (char '.')

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- decimal
    char '.'
    minor <- decimal
    char '.'
    patch <- decimal
    release <- (char '-' >> parseNOSs) <|> pure []
    metadata <- (char '+' >> parseNOSs) <|> pure []
    return (SemVer major minor patch release metadata)

--------------------------------------------------------------------------------
parseDigit :: Parser Char
parseDigit = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
    sign <- char '+' <|> char '-' <|> pure '+'
    i <- base10Integer
    return ((if sign == '+' then 1 else (-1)) * i)

testDecimal :: IO ()
testDecimal = do
    print $ parseString parseDigit mempty "123"
    print $ parseString parseDigit mempty "abc"
    print $ parseString base10Integer mempty "123abc"
    print $ parseString base10Integer mempty "abc"
    print $ parseString base10Integer' mempty "-123abc"

--------------------------------------------------------------------------------
type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
    _ <- many parseSep
    _ <- try ((parseIntLimited 1 :: Parser Int) >> some parseSep) <|> pure " "
    _ <- many parseSep
    numberingPlanArea <- parseIntLimited 3
    _ <- many parseSep
    exchange <- parseIntLimited 3
    _ <- many parseSep
    lineNumber <- parseIntLimited 4
    return (PhoneNumber numberingPlanArea exchange lineNumber)
    where
        parseSep = oneOf "()- "
        parseIntLimited n = read <$> count n parseDigit

testPhone :: IO ()
testPhone = do
    print $ parseString parsePhone mempty "123-456-7890"
    -- Success (PhoneNumber 123 456 7890)
    print $ parseString parsePhone mempty "1234567890"
    -- Success (PhoneNumber 123 456 7890)
    print $ parseString parsePhone mempty "(123) 456-7890"
    -- Success (PhoneNumber 123 456 7890)
    print $ parseString parsePhone mempty "1-123-456-7890"
    -- Success (PhoneNumber 123 456 7890)
        
