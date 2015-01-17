{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import UDPServer
import ParsedLogData

import System.IO
import qualified Network.Socket as NS

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

import qualified Data.Conduit.Network.UDP as DCU
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Attoparsec as CATT

import qualified Data.Attoparsec.ByteString as ATT

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

maxPacketSize = 5000

-- Use these proceeding three functions for live UDP servers
messageProducer :: NS.Socket -> C.Source IO DCU.Message
messageProducer server = DCU.sourceSocket server maxPacketSize

msgToBS :: C.Conduit DCU.Message IO BS.ByteString
msgToBS = CL.map DCU.msgData

udpPrinter :: C.Sink DCU.Message IO ()
udpPrinter = C.awaitForever $ liftIO . printMessageData
  where
    printMessageData = BS.putStrLn . DCU.msgData

-- Test locally against a log file
fileToBS :: IO Handle -> C.Source (ResourceT IO) BS.ByteString
fileToBS handleMaker = source C.$= bsSplitterConduit
  where source = CB.sourceIOHandle handleMaker
        bsSplitterConduit = CB.lines

bsPrinter :: C.Sink BS.ByteString (ResourceT IO) ()
bsPrinter = CL.mapM_ $ liftIO . BS.putStrLn

--parseToLogData :: (Monad m, CATT.AttoparsecInput a) => ATT.Parser a -> C.Conduit BS.ByteString IO (Either LogData (CATT.PositionRange, b))
parseToLogData :: ATT.Parser BS.ByteString -> C.Conduit a0 m0 (Either CATT.ParseError (CATT.PositionRange, b0))
parseToLogData = CATT.conduitParserEither 

-- Test file generator
fileWriter :: Handle -> C.Sink BS.ByteString IO ()
fileWriter handle = C.awaitForever $ liftIO . (writeMessageData handle)
  where
    writeMessageData handle = BS.hPutStrLn handle

syslogReceiverMain :: IO ()
syslogReceiverMain = do
  udpServer <- newSyslogListener
  let udpMessages = messageProducer udpServer

  syslogFile <- openFile "syslog.log" WriteMode
  let logWriter = fileWriter syslogFile

  udpMessages C.$= msgToBS C.$$ logWriter

staticFileReaderMain :: IO ()
staticFileReaderMain = do

  let openAction = openFile "syslog.log" ReadMode
  runResourceT $ fileToBS openAction C.$$ bsPrinter

main :: IO ()
main = staticFileReaderMain