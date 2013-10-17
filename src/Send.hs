{-# LANGUAGE ForeignFunctionInterface #-}

module Send (
    bsSend
  , rawSend
  ) where

import Control.Monad (when)
import Data.ByteString.Internal
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import GHC.Conc (threadWaitWrite)
import Network.Socket (Socket(..))
import qualified Network.Socket.ByteString as SB (sendAll)
import System.Posix.Types (Fd(..))

import Types

----------------------------------------------------------------

bsSend :: Socket -> ByteString -> Sender
bsSend  = SB.sendAll

----------------------------------------------------------------

rawSend :: Socket -> ByteString -> Sender
rawSend sock bs = withForeignPtr fptr $ \ptr -> do
    let buf = castPtr (ptr `plusPtr` off)
    sendloop s buf siz
  where
    MkSocket s _ _ _ _ = sock
    PS fptr off len = bs
    siz = fromIntegral len

sendloop :: CInt -> Ptr CChar -> CSize -> IO ()
sendloop s buf len = do
    bytes <- c_send s buf len 0
    if bytes == -1 then do
        errno <- getErrno
        if errno == eAGAIN then do
            threadWaitWrite (Fd s)
            sendloop s buf len
          else
            throwErrno "sendloop"
      else do
        let sent = fromIntegral bytes
        when (sent /= len) $ do
            let left = len - sent
                ptr = buf `plusPtr` fromIntegral bytes
            sendloop s ptr left

foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt

