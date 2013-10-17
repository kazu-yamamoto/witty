module Worker where

import Control.Concurrent (yield)
import Control.Monad (when)
import Network.Socket (Socket, sClose)

import Buffer
import Malloc
import Recv
import Reply
import Send
import Types

worker :: Options -> Arena -> Socket -> IO ()
worker opt arena sock = do
    (receiver, closer) <- receiverCloser
    serve opt receiver sender closer
  where
    sender
      | useRawSend opt = rawSend sock reply
      | otherwise      = bsSend sock reply
    receiverCloser
      | useMalloc opt  = do
          buf <- mallocBuf recvBufferSize
          let rec = rawRecvM sock buf recvBufferSize
              clo = do
                  sClose sock
                  freeBuf buf
          return (rec, clo)
      | useRawRecv opt = do
          rbuf <- if prepareRecvBuf opt then
                      borrowBuffer arena
                  else
                      getBuffer
          let rec = rawRecv sock rbuf recvBufferSize
              clo = do
                  sClose sock
                  when (prepareRecvBuf opt) $ returnBuffer arena rbuf
          return (rec, clo)
      | otherwise = do
          let rec = bsRecv sock recvBufferSize
          return (rec, sClose sock)

serve :: Options -> Receiver -> Sender -> Closer -> IO ()
serve opt receiver sender closer = do
    len <- receiver
    if len > 0 then do
        sender
        when (yieldAfterSend opt) yield
        serve opt receiver sender closer
      else
        closer
