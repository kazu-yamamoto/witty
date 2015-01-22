module Accept (acceptLoop) where

import Control.Concurrent (forkFinally, runInUnboundThread)
import qualified Control.Exception as E
import Control.Monad (void)
import Network (sClose)
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
        void $ forkFinally (worker opt arena sock) (const $ sClose sock)
        loop
    handler :: E.IOException -> IO ()
    handler e = print e
