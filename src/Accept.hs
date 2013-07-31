module Accept (acceptLoop) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Network.Socket (Socket, accept)

import Worker

acceptLoop :: Socket -> IO ()
acceptLoop s = forever $ do
    (sock, _) <- accept s
    forkIO $ worker sock
