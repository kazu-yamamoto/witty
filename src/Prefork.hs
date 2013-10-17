module Prefork where

import Control.Exception as E (SomeException, catch)
import Control.Monad (replicateM, void)
import Network (Socket)
import System.Exit (ExitCode(..))
import System.Posix.Process (forkProcess, exitImmediately)
import System.Posix.Signals (Signal, Handler(Catch,Ignore), sigTERM, sigINT, sigCHLD, installHandler, signalProcess)
import System.Posix.Types (ProcessID)

import Accept
import Types

prefork :: Options -> Socket -> IO ()
prefork opt s
  | n <= 0    = error "prefork"
  | n == 1    = return ()
  | otherwise = do
      pids <- replicateM (n-1) $ forkProcess $ acceptLoop opt s
      ignoreSigChild
      setHandler sigTERM $ stopHandler pids
      setHandler sigINT  $ stopHandler pids
  where
    n = processes opt

setHandler :: Signal -> Handler -> IO ()
setHandler sig func = void $ installHandler sig func Nothing

stopHandler :: [ProcessID] -> Handler
stopHandler pids = Catch $ do
    mapM_ (sendSignal sigTERM) pids
    exitImmediately ExitSuccess

sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `E.catch` ignore

ignore :: SomeException -> IO ()
ignore _ = return ()

ignoreSigChild :: IO ()
ignoreSigChild = setHandler sigCHLD Ignore
