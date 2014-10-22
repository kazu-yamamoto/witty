module Accept (acceptLoop) where

import Control.Concurrent (forkIO, runInUnboundThread)
import Control.Monad (forever, void)
import Network.Socket (Socket, accept, sClose)
import Control.Exception as E

import Buffer
import Types
import Worker

acceptLoop :: Options -> Socket -> IO ()
acceptLoop opt s = do
    arena <- if prepareRecvBuf opt then
                 prepareArena
             else
                 prepareDummyArena
    run $ acceptLoop' opt arena s
  where
    run
      | acceptInUnbound opt = runInUnboundThread
      | otherwise           = id

acceptLoop' :: Options -> Arena -> Socket -> IO ()
acceptLoop' opt arena s = forever $ handle handler $ do
    (sock, _) <- accept s
    void . forkIO $ worker opt arena sock
  where
    handler :: SomeException -> IO ()
    handler _ = sClose s
