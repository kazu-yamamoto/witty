module Worker where

import Control.Monad (when)
import Data.ByteString.Internal (toForeignPtr)
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr)
import Network.Socket (Socket)

import Http
import Send
import Recv

recvBufferSize :: Int
recvBufferSize = 4096

worker :: Socket -> IO ()
worker sock = do
    recvBuffer <- mallocForeignPtrBytes recvBufferSize
    let (replyFPtr,_,_) = toForeignPtr reply
    withForeignPtr replyFPtr $
        withForeignPtr recvBuffer . serve sock

serve :: Socket -> Ptr Word8 -> Ptr Word8 -> IO ()
serve sock replyPtr recvPtr = do
    len <- recv sock recvPtr recvBufferSize
    when (len > 0) $ do
        sendAll sock replyPtr replyLen
        serve sock replyPtr recvPtr
