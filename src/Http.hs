{-# LANGUAGE OverloadedStrings #-}

module Http (
    expectedRequest
  , expectedRequestLength
  , reply
  , replyLen
  ) where

import Data.ByteString as B
import Data.ByteString.Char8 ()

-- MODIFY HERE

expectedIPandPort :: ByteString
expectedIPandPort = "10.12.0.1:8080"

-- EXPECTED REQUEST
expectedRequest :: ByteString
expectedRequest = B.concat [
    "GET / HTTP/1.1\r\n"
  , "Host: ", expectedIPandPort, "\r\n"
  , "User-Agent: weighttp/0.3\r\n"
  , "Connection: keep-alive\r\n"
  , "\r\n"
  ]

expectedRequestLength :: Int
expectedRequestLength = B.length expectedRequest

-- REPLY
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

replyLen :: Int
replyLen = B.length reply
