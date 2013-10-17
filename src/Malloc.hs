module Malloc where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (mallocBytes, free)

mallocBuf :: Int -> IO (Ptr Word8)
mallocBuf = mallocBytes

freeBuf :: Ptr Word8 -> IO ()
freeBuf = free
