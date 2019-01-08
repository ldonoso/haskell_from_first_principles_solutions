{-# LANGUAGE QuasiQuotes #-}

module DailyLog where

import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ
import Text.Pretty.Simple (pPrint)

sample :: String
sample = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

newtype Des = Des String deriving (Show)
data ScheduledTask = ScheduledTask { time :: Integer, des :: Des } deriving (Show)

newtype Date = Date String deriving (Show)
data DailyLog = DailyLog { date :: Date,  tasks :: [ScheduledTask]} deriving (Show)

parseTime :: Parser Integer
parseTime = do
    hour <- integer
    char ':'
    min <- integer
    return (hour * 60 + min)


parseDes :: Parser Des
parseDes = Des <$> ((try $ manyTill (noneOf "\n") parseComment) <|> (many $ noneOf "\n"))

parseScheduledTask = do
    time <- parseTime
    des <- token parseDes
    skipMany parseComment
    return $ ScheduledTask time des

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseComment :: Parser ()
parseComment = string "--" >> (skipMany $ notChar '\n') >> skipEOL

parseDailyLogHeader :: Parser Date
parseDailyLogHeader = do
    token (char '#')
    date <- token $ some $ noneOf " \n"
    skipOptional parseComment
    return $ Date date

parseDailyLog :: Parser DailyLog
parseDailyLog = do
    skipEOL
    many parseComment
    header <- token parseDailyLogHeader
    tasks <- some parseScheduledTask
    return (DailyLog header tasks)

parseLog :: Parser [DailyLog]
parseLog = some parseDailyLog

timePerTask :: [ScheduledTask] -> [Integer]
timePerTask tasks = zipWith (\t1 t2 -> (time t1) - (time t2)) (drop 1 tasks) tasks

test :: IO ()
test = do
    let logs = parseString parseLog mempty sample
    pPrint logs

    let times = (fmap . fmap) (timePerTask . tasks) logs
    pPrint times

    let avg = (fmap . fmap) (\x -> (fromIntegral . sum) x / (fromIntegral . length) x) times
    pPrint avg
