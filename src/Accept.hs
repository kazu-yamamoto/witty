module Accept (acceptLoop) where

import Control.Concurrent (forkIO, runInUnboundThread)
import qualified Control.Exception as E
import Control.Monad (void)
import Network.Socket (Socket, accept)

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
acceptLoop' opt arena s = E.handle handler loop
  where
    loop = do
        (sock, _) <- accept s
        void . forkIO $ worker opt arena sock
        loop
    handler :: E.IOException -> IO ()
    handler e = print e
