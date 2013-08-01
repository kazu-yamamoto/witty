module Buffer (
    Arena
  , prepareArena
  , prepareDummyArena
  , getBuffer
  , borrowBuffer
  , returnBuffer
  ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Foreign.ForeignPtr (mallocForeignPtrBytes)

import Types

newtype Arena = Arena (IORef [Buffer])

getBuffer :: IO Buffer
getBuffer = mallocForeignPtrBytes recvBufferSize

prepareArena :: IO Arena
prepareArena = do
    bufs <- replicateM arenaSize getBuffer
    Arena <$> newIORef bufs

prepareDummyArena :: IO Arena
prepareDummyArena = Arena <$> newIORef []

borrowBuffer :: Arena -> IO Buffer
borrowBuffer (Arena ref) = do
    mbuf <- atomicModifyIORef ref borrow
    case mbuf of
        Nothing  -> getBuffer
        Just buf -> return buf
  where
    borrow []     = ([], Nothing)
    borrow (b:bs) = (bs, Just b)

returnBuffer :: Arena -> Buffer -> IO ()
returnBuffer (Arena ref) buf = atomicModifyIORef ref $ \bs -> (buf:bs, ())
