module Types where

data Options = Options {
    acceptInUnbound :: Bool
  , yieldAfterSend  :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
    acceptInUnbound = False
  , yieldAfterSend  = False
  }
