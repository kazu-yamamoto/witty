module Main where

import System.Console.GetOpt
import System.Environment

import Accept
import Listen
import Types

options :: [OptDescr Flag]
options = [
    Option ['a'] ["accept"] (NoArg AcceptInUnbound) "accept loop in unbound thread"
  , Option ['y'] ["yield"] (NoArg Yield) "yield after sending a reply to give time to the next coming request"
  ]

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argv =  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: witty [OPTION] <port>"

main :: IO ()
main = do
    (flags,[port]) <- getArgs >>= parseArgs
    listenSocket port >>= acceptLoop flags
