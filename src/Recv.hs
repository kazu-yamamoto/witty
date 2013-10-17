{-# LANGUAGE ForeignFunctionInterface #-}

module Recv (
    bsRecv
  , rawRecv
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadWaitRead)
import qualified Data.ByteString as B (length)
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Network.Socket (Socket, fdSocket)
import qualified Network.Socket.ByteString as SB (recv)
import System.Posix.Types (Fd(..))

import Types

----------------------------------------------------------------

bsRecv :: Socket -> Int -> Receiver
bsRecv sock size = B.length <$> SB.recv sock size

----------------------------------------------------------------

rawRecv :: Socket -> Buffer -> Int -> Receiver
rawRecv sock fptr len = withForeignPtr fptr $ \ptr -> do
    let buf = castPtr ptr
    fromIntegral <$> recvloop s buf size
  where
    s = fdSocket sock
    size = fromIntegral len

recvloop :: CInt -> Ptr CChar -> CSize -> IO CInt
recvloop sock buf size = do
    bytes <- c_recv sock buf size 0
    if bytes == -1 then do
        errno <- getErrno
        if errno == eAGAIN then do
            threadWaitRead (Fd sock)
            recvloop sock buf size
          else
            throwErrno "recvloop"
       else
        return bytes

foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
