module Accept (acceptLoop) where

import Control.Concurrent (forkIO, runInUnboundThread)
import Control.Monad (forever, void)
import Network.Socket (Socket, accept, sClose)
import Control.Exception as E

import Worker
import Types

acceptLoop :: Options -> Socket -> IO ()
acceptLoop opt s
  | acceptInUnbound opt = runInUnboundThread $ acceptLoop' opt s
  | otherwise           = acceptLoop' opt s

acceptLoop' :: Options -> Socket -> IO ()
acceptLoop' opt s = handle handler $ forever $ do
    (sock, _) <- accept s
    void . forkIO $ worker opt sock
  where
    handler :: SomeException -> IO ()
    handler _ = sClose s
