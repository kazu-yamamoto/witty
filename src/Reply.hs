{-# LANGUAGE OverloadedStrings #-}

module Reply (
    reply
  , replySize
  , replyBuffer
  ) where

import Data.ByteString as B (ByteString, concat, length)
import Data.ByteString.Char8 ()
import Data.ByteString.Internal (ByteString(..))

import Types

reply :: ByteString
reply = B.concat [
    "HTTP/1.1 200 OK\r\n"
  , "Date: Tue, 09 Oct 2012 16:36:18 GMT\r\n"
  , "Content-Length: 151\r\n"
  , "Server: Witty/0.0.0\r\n"
  , "Last-Modified: Mon, 09 Jul 2012 03:42:33 GMT\r\n"
  , "Content-Type: text/html\r\n"
  , "\r\n"
  , "<html>\n"
  , "<head>\n"
  , "<title>Welcome to witty!</title>\n"
  , "</head>\n"
  , "<body bgcolor=\"white\" text=\"black\">\n"
  , "<center><h1>Welcome to witty!</h1></center>\n"
  , "</body>\n"
  , "</html>\n"
  ]

replySize :: Int
replySize = B.length reply

replyBuffer :: Buffer
replyBuffer = buf
  where
    PS buf _ _ = reply
