module Worker where

import Control.Concurrent (yield)
import Control.Monad (when)
import Foreign.ForeignPtr (mallocForeignPtrBytes)
import Network.Socket (Socket, sClose)

import Recv
import Reply
import Send
import Types

recvBufferSize :: Int
recvBufferSize = 4096

worker :: Options -> Socket -> IO ()
worker opt sock = do
    receiver <- if useRawRecv opt then do
                    recvBuffer <- mallocForeignPtrBytes recvBufferSize
                    return $ rawRecv sock recvBuffer recvBufferSize
                else
                    return $ bsRecv sock recvBufferSize
    let sender
          | useRawSend opt = rawSend sock replyBuffer replySize
          | otherwise      = bsSend sock reply
        closer = sClose sock
    serve opt receiver sender closer

serve :: Options -> Receiver -> Sender -> Closer -> IO ()
serve opt receiver sender closer = do
    len <- receiver
    if len > 0 then do
        sender
        when (yieldAfterSend opt) yield
        serve opt receiver sender closer
      else
        closer
