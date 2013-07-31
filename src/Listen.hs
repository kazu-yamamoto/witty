module Listen (listenSocket) where

import Network.Socket

listenQueueLength :: Int
listenQueueLength = 2048

listenSocket :: String -> IO Socket
listenSocket portNumber = do
    let hint = defaultHints {addrFlags = [AI_PASSIVE]}
    addrinfos <- getAddrInfo (Just hint) Nothing (Just portNumber)
    let serveraddr = head addrinfos
    listenSock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket listenSock $ addrAddress serveraddr
    setSocketOption listenSock ReuseAddr 1
    setSocketOption listenSock NoDelay 1
    listen listenSock listenQueueLength
    return listenSock
