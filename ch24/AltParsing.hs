{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ
import Text.Fractions (virtuousFraction)

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = do
    skipMany (oneOf "\n")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

type DecimalOrRational = Either Rational Integer

parseDecimalOrRational :: Parser DecimalOrRational
parseDecimalOrRational = do
    skipMany (oneOf "\n")
    v <- (Left <$> try virtuousFraction) <|> (Right <$> decimal)
    skipMany (oneOf "\n")
    return v

eitherOr2 = [r|
123
12/23
1/0
|]

main = do
    print $ parseString (some parseNos) mempty eitherOr
    print $ parseString (some $ token parseDecimalOrRational) mempty eitherOr2
