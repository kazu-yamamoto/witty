module Main where

import System.Environment

import Listen
import Accept

main :: IO ()
main = do
    [port] <- getArgs
    listenSocket port >>= acceptLoop
