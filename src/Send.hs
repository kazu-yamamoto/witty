{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Send (
    bsSend
  , rawSend
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadWaitWrite)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Network.Socket (Socket(..))
import qualified Network.Socket.ByteString as SB (sendAll)
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)

import Types

----------------------------------------------------------------

bsSend :: Socket -> ByteString -> Sender
bsSend  = SB.sendAll

----------------------------------------------------------------

rawSend :: Socket -> Buffer -> Int -> IO ()
rawSend sock buf size = withForeignPtr buf $ \ptr -> sendAll sock ptr size

sendAll :: Socket -> Ptr a -> Int -> IO ()
sendAll !sock !ptr !len = do
    sent <- send' sock ptr len
    when (sent < len) $ sendAll sock (ptr `plusPtr` sent) (len - sent)

send' :: Socket -> Ptr a -> Int -> IO Int
send' (MkSocket s _ _ _ _) ptr len =
    fromIntegral <$> wrap "send'" fallback action
  where
    wrap = throwSocketErrorIfMinus1RetryMayBlock
    fallback = threadWaitWrite $ fromIntegral s
    action = c_send s ptr (fromIntegral len) 0

foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt

