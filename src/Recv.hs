{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Recv (
    bsRecv
  , rawRecv
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadWaitRead)
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Network.Socket (Socket, fdSocket)
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)
import qualified Network.Socket.ByteString as SB (recv)
import qualified Data.ByteString as B (length)

import Types

----------------------------------------------------------------

bsRecv :: Socket -> Int -> Receiver
bsRecv sock size = B.length <$> SB.recv sock size

----------------------------------------------------------------

rawRecv :: Socket -> Buffer -> Int -> Receiver
rawRecv sock buf size = withForeignPtr buf $ \ptr -> recv sock ptr size

recv :: Socket -> Ptr a -> Int -> IO Int
recv sock ptr !nbytes
  | nbytes < 0 = error "recv"
  | otherwise  = recv' (fdSocket sock) nbytes ptr

recv' :: CInt -> Int -> Ptr a -> IO Int
recv' s !nbytes ptr = fromIntegral <$> wrap "recv'" fallback action
  where
    wrap = throwSocketErrorIfMinus1RetryMayBlock
    fallback = threadWaitRead $ fromIntegral s
    action = c_recv s (castPtr ptr) (fromIntegral nbytes) 0

foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
