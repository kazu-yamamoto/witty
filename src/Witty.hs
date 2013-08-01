module Main where

import Control.Monad (when)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Accept
import Listen
import Types

options :: [OptDescr (Options -> Options)]
options = [
    Option ['a']
           ["accept"]
           (NoArg $ \opt -> opt { acceptInUnbound = True } )
           "accept loop in unbound thread"
  , Option ['y']
           ["yield"]
           (NoArg $ \opt -> opt { yieldAfterSend = True } )
           "yield after sending a reply to give time to the next coming request"
  , Option ['s']
           ["send"]
           (NoArg $ \opt -> opt { useRawSend = True } )
           "directly send a buffer"
  , Option ['r']
           ["recv"]
           (NoArg $ \opt -> opt { useRawRecv = True } )
           "directly receive a packet to a buffer"
  , Option ['p']
           ["prepare"]
           (NoArg $ \opt -> opt { prepareRecvBuf = True } )
           "prepare receive buffer in advance"
  ]

parseArgs :: [String] -> IO (Options, [String])
parseArgs argv =  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: witty [OPTION] <port>"

main :: IO ()
main = do
    (opt,[port]) <- getArgs >>= parseArgs
    when (prepareRecvBuf opt && not (useRawRecv opt)) $ do
        hPutStrLn stderr "'-p' requires '-r'."
        exitFailure
    listenSocket port >>= acceptLoop opt
