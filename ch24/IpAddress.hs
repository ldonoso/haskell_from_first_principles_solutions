module IpAddress where

import Data.Word
import Text.Trifecta
import Numeric (readHex)

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

parseIpAddress :: Parser IPAddress
parseIpAddress = do
    b1 <- integer
    char '.'
    b2 <- integer
    char '.'
    b3 <- integer
    char '.'
    b4 <- integer

    return $ IPAddress $ fromIntegral $ b1 * 2^24 + b2 * 2^16 + b3 * 2^8 + b4

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

hexToInteger :: String -> Integer
hexToInteger = fst . (!! 0) . readHex

expandBytes :: [String] -> [String]
expandBytes bytes = go (8 - length bytes) bytes where
    go _ [] = []
    go 0 l@("":xs) = "0" : (go 0 xs)
    go 0 xs = xs
    go n l@("":xs) = "0" : (go (n - 1) l)
    go n (x:xs) = x : (go n xs)

parseIpAddress6 :: Parser Integer
parseIpAddress6 = do
    bytes <- expandBytes <$> sepBy (many hexDigit) (char ':')
    let bytes_num = hexToInteger <$> bytes
    return $ fromBytes 0 bytes_num
    where 
        fromBytes acc [] = acc
        fromBytes acc (x:xs) = fromBytes (acc * 2^16 + x) xs

testExpand :: IO ()
testExpand = print $ expandBytes ["1", "", "2"]
