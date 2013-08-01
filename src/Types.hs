module Types where

import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

data Options = Options {
    acceptInUnbound :: Bool
  , yieldAfterSend  :: Bool
  , useRawSend      :: Bool
  , useRawRecv      :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
    acceptInUnbound = False
  , yieldAfterSend  = False
  , useRawSend      = False
  , useRawRecv      = False
  }

type Receiver = IO Int
type Sender = IO ()
type Closer = IO ()

type Buffer = ForeignPtr Word8
