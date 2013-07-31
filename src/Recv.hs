{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Recv (recv) where

import Control.Concurrent (threadWaitRead)
import Foreign
import Foreign.C.Types
import Network.Socket (Socket, fdSocket)
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)

recv :: Socket         -- ^ Connected socket
        -> Ptr Word8
        -> Int         -- ^ Maximum number of bytes to receive
        -> IO Int
recv sock ptr !nbytes
  | nbytes < 0 = error "recv"
  | otherwise  = recvInner (fdSocket sock) nbytes ptr

recvInner :: CInt -> Int -> Ptr Word8 -> IO Int
recvInner s !nbytes ptr =
    fmap fromIntegral $
        throwSocketErrorIfMinus1RetryMayBlock "recvInner"
        (threadWaitRead (fromIntegral s)) $
        c_recv s (castPtr ptr) (fromIntegral nbytes) 0

foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
