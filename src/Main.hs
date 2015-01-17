module Main (main) where

import System.IO
import UDPServer
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.Network.UDP as DCU
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 as BS

import qualified Network.Socket as NS


maxPacketSize = 5000

messageProducer :: NS.Socket -> C.Source IO DCU.Message
messageProducer server = sourceSocket server maxPacketSize

printer :: C.Sink DCU.Message IO ()
printer = C.awaitForever $ liftIO . printMessageData
  where
    printMessageData = BS.putStrLn . DCU.msgData

msgToBS :: C.Conduit DCU.Message IO BS.ByteString
msgToBS = CL.map DCU.msgData

fileWriter :: Handle -> C.Sink BS.ByteString IO ()
fileWriter handle = C.awaitForever $ liftIO . (writeMessageData handle)
  where
    writeMessageData handle = BS.hPutStrLn handle

main :: IO ()
main = do
  udpServer <- newSyslogListener
  let udpMessages = messageProducer udpServer

  logFile <- openFile "syslog.log" ReadMode
  let logWriter = fileWriter logFile

  udpMessages C.$= msgToBS C.$$ logWriter
