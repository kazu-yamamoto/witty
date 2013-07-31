module Accept (acceptLoop) where

import Control.Concurrent (forkIO, runInUnboundThread)
import Control.Monad (forever)
import Network.Socket (Socket, accept)

import Worker
import Types

acceptLoop :: Options -> Socket -> IO ()
acceptLoop opt s
  | acceptInUnbound opt = runInUnboundThread $ acceptLoop' opt s
  | otherwise           = acceptLoop' opt s

acceptLoop' :: Options -> Socket -> IO ()
acceptLoop' opt s = forever $ do
    (sock, _) <- accept s
    forkIO $ worker opt sock
