module Main where

import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.Environment (getArgs)

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
    listenSocket port >>= acceptLoop opt
