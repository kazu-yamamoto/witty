module Worker where

import Control.Concurrent (yield)
import Control.Monad (when)
import Data.ByteString.Internal (toForeignPtr)
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr)
import Network.Socket (Socket, sClose)

import Http
import Recv
import Send
import Types

recvBufferSize :: Int
recvBufferSize = 4096

worker :: Options -> Socket -> IO ()
worker opt sock = do
    recvBuffer <- mallocForeignPtrBytes recvBufferSize
    let (replyFPtr,_,_) = toForeignPtr reply
        shouldYield = yieldAfterSend opt
    withForeignPtr replyFPtr $
        withForeignPtr recvBuffer . serve shouldYield sock

serve :: Bool -> Socket -> Ptr Word8 -> Ptr Word8 -> IO ()
serve shouldYield sock replyPtr recvPtr = loop
  where
    loop = do
        len <- recv sock recvPtr recvBufferSize
        if len > 0 then do
            sendAll sock replyPtr replyLen
            when shouldYield yield
            loop
          else
            sClose sock

