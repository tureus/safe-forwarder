module UDPServer (newSyslogListener) where

import qualified Network.Socket as NS

data Server = UDPServer {socket :: NS.Socket}

syslogPort = "4300"
queueDepth = 1000

newUDPSocket :: IO (NS.Socket)
newUDPSocket = NS.socket fam stype protnum
  where
    fam = NS.AF_INET
    stype = NS.Datagram
    protnum = NS.defaultProtocol

newSyslogListener :: IO (NS.Socket)
newSyslogListener = do
  addrinfos <- NS.getAddrInfo 
               (Just (NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE]}))
               Nothing (Just syslogPort)
  let serveraddr = head addrinfos

  udpSocket <- newUDPSocket

  NS.bindSocket udpSocket (NS.addrAddress serveraddr)

  return udpSocket

hi :: String
hi = "hi"