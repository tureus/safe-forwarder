module Main (main) where

import UDPServer
import qualified Data.Conduit as DC
import Data.Conduit.Network.UDP as DCU
import Control.Monad.Trans (liftIO)

import qualified Network.Socket as NS


maxPacketSize = 5000

producer :: NS.Socket -> DC.Source IO DCU.Message
producer server = sourceSocket server maxPacketSize

consumer :: DC.Sink DCU.Message IO ()
consumer = DC.awaitForever $ liftIO . printMessageData
  where
    printMessageData message = putStrLn . show $ DCU.msgData message

main :: IO ()
main = do
  udpServer <- newSyslogListener
  let udpMessages = producer udpServer
  udpMessages DC.$$ consumer
