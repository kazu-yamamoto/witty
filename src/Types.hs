module Types where

import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

data Options = Options {
    acceptInUnbound :: Bool
  , yieldAfterSend  :: Bool
  , useRawSend      :: Bool
  , useRawRecv      :: Bool
  , prepareRecvBuf  :: Bool
  , processes       :: Int
  }

defaultOptions :: Options
defaultOptions = Options {
    acceptInUnbound = False
  , yieldAfterSend  = False
  , useRawSend      = False
  , useRawRecv      = False
  , prepareRecvBuf  = False
  , processes       = 1
  }

type Receiver = IO Int
type Sender = IO ()
type Closer = IO ()

type Buffer = ForeignPtr Word8

recvBufferSize :: Int
recvBufferSize = 4096

arenaSize :: Int
arenaSize = 1000
