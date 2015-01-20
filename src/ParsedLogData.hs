{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParsedLogData
where

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString as ATT

import System.Locale
import Data.Time
import Data.Time.Format

syslogDateFormat = "%b %d %l:%M:%S"

data LogData = LogData {
  pri :: Priority,
  timestamp :: Maybe UTCTime,
  source :: IPAddress,
  message :: BS.ByteString
} deriving (Show)
data Priority = RawPriority BS.ByteString deriving (Show)
type IPAddress = (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)

--instance Show (ParseTime) where
--  show time = "hey"

syslogTestString :: BS.ByteString
syslogTestString = "<164>Jan 17 01:58:58 10.11.10.40 %ASA-4-419002: Duplicate TCP SYN from outside2:104.237.142.206/80 to pub_uag:8.37.98.29/443 with different initial sequence number"

--syslogParser :: Parser (Priority, Maybe UTCTime, IPAddress, BS.ByteString)
syslogParser = do
  pri <- priority <?> "priority parse error"
  mbDate <- date <?> "date parse error"
  space
  srcAddr <- ip
  space
  msg <- ATT.takeByteString
  return LogData{pri = pri, timestamp = mbDate, source = srcAddr, message = msg}

priority :: Parser Priority
priority = do
  string "<"
  digitsString <- takeWhile1 digit
  string ">"
  return (RawPriority digitsString)

date :: Parser (Maybe UTCTime)
date = do
  rawDate <- ATT.take 15
  let stringDate = BS.unpack rawDate
  let parsedDate = parseTime defaultTimeLocale syslogDateFormat stringDate
  return parsedDate

ip :: Parser IPAddress
ip = do
  oct0 <- takeWhile1 digit
  period
  oct1 <- takeWhile1 digit
  period
  oct2 <- takeWhile1 digit
  period
  oct3 <- takeWhile1 digit
  return (oct0, oct1, oct2, oct3)
--ip = takeWhile1 (\x -> digit x || x == 46)

space = string " "
colon = string ":"
period = string "."

digit test = (test >= 48 && test <= 57)
octet = digit