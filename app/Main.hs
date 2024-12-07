module Main where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as B
import Network.Socket
import Network.Socket.ByteString (sendAll)

import qualified HttpMsg as H

runTcpServer :: (Socket -> IO a) -> IO a
runTcpServer server_loop = E.bracket initSock close loop
  where
    addr = (SockAddrInet 8080 (tupleToHostAddress (127, 0, 0, 1)))
    initSock = E.bracketOnError (socket AF_INET Stream 0) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      bind sock addr
      listen sock 1
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _addr) ->
          void $
            forkFinally (server_loop conn) (const $ gracefulClose conn 5000)

main :: IO ()
main = runTcpServer $ \con ->
  sendAll con $
    B.pack $ show $
      H.Response
        (H.StatusLine (H.HttpVer 1 1) H.S200)
        (H.GeneralHeader [])
        (H.ResponseHeader [])
        (H.EntityHeader [])
        ("Hello from server!")
