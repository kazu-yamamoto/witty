module Worker where

import Control.Concurrent (yield)
import Control.Monad (when)
import Data.ByteString.Internal (toForeignPtr)
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr)
import Network.Socket (Socket)

import Http
import Recv
import Send
import Types

recvBufferSize :: Int
recvBufferSize = 4096

worker :: [Flag] -> Socket -> IO ()
worker flags sock = do
    recvBuffer <- mallocForeignPtrBytes recvBufferSize
    let (replyFPtr,_,_) = toForeignPtr reply
        shouldYield = Yield `elem` flags
    withForeignPtr replyFPtr $
        withForeignPtr recvBuffer . serve shouldYield sock

serve :: Bool -> Socket -> Ptr Word8 -> Ptr Word8 -> IO ()
serve shouldYield sock replyPtr recvPtr = loop
  where
    loop = do
        len <- recv sock recvPtr recvBufferSize
        when (len > 0) $ do
            sendAll sock replyPtr replyLen
            when shouldYield yield
            loop

