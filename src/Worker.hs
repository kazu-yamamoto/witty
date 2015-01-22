module Worker where

import Control.Concurrent (yield)
import Control.Exception (finally)
import Control.Monad (when)
import Network.Socket (Socket)

import Buffer
import Malloc
import Recv
import Reply
import Send
import Types


worker :: Options -> Arena -> Socket -> IO ()
worker opt arena sock = do
    (receiver, closer) <- receiverCloser
    serve opt receiver sender `finally` closer
  where
    sender
      | useRawSend opt = rawSend sock reply
      | otherwise      = bsSend sock reply
    receiverCloser
      | useMalloc opt  = do
          buf <- mallocBuf recvBufferSize
          let rec = rawRecvM sock buf recvBufferSize
              clo = freeBuf buf
          return (rec, clo)
      | useRawRecv opt = do
          rbuf <- if prepareRecvBuf opt then
                      borrowBuffer arena
                  else
                      getBuffer
          let rec = rawRecv sock rbuf recvBufferSize
              clo = when (prepareRecvBuf opt) $ returnBuffer arena rbuf

          return (rec, clo)
      | otherwise = do
          let rec = bsRecv sock recvBufferSize
          return (rec, return ())

serve :: Options -> Receiver -> Sender -> IO ()
serve opt receiver sender = loop
  where
    loop = do
        len <- receiver
        when (len > 0) $ do
            sender
            when (yieldAfterSend opt) yield
            loop
