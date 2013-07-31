{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Send (sendAll) where

import Control.Concurrent (threadWaitWrite)
import Control.Monad (when, liftM)
import Foreign
import Foreign.C.Types
import Network.Socket (Socket(..))
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)

sendAll :: Socket      -- ^ Connected socket
           -> Ptr a
           -> Int
           -> IO ()
sendAll !sock !ptr !len = do
    sent <- send' sock ptr len
    when (sent < len) $ sendAll sock (ptr `plusPtr` sent) (len - sent)

send' :: Socket         -- ^ Connected socket
         -> Ptr a       -- ^ Pointer to beginning of data to send
         -> Int         -- ^ Amount of data to send
         -> IO Int      -- ^ Number of bytes sent
send' (MkSocket s _ _ _ _) ptr len =
    liftM fromIntegral $
        throwSocketErrorIfMinus1RetryMayBlock "send'"
        (threadWaitWrite $ fromIntegral s) $
        c_send s ptr (fromIntegral len) 0

foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt

