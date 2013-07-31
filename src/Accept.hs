module Accept (acceptLoop) where

import Control.Concurrent (forkIO, runInUnboundThread)
import Control.Monad (forever)
import Network.Socket (Socket, accept)

import Worker
import Types

acceptLoop :: [Flag] -> Socket -> IO ()
acceptLoop flags s
  | AcceptInUnbound `elem` flags = runInUnboundThread $ acceptLoop' flags s
  | otherwise                    = acceptLoop' flags s

acceptLoop' :: [Flag] -> Socket -> IO ()
acceptLoop' flags s = forever $ do
    (sock, _) <- accept s
    forkIO $ worker flags sock
