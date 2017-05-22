{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Gopher hiding (crlf)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

port :: Integer
port = 70

main :: IO ()
main = withSocketsDo $ do
  addr <- getAddrInfo (Just defaultHints) Nothing (Just $ show port)
  sock <- socket (addrFamily $ head addr) Stream defaultProtocol
  bind sock (addrAddress $ head addr)
  listen sock 1
  acceptLoop sock
  close sock

acceptLoop :: Socket -> IO ()
acceptLoop sock = do
  (conn, _) <- accept sock
  gopherSession conn
  close conn
  acceptLoop sock

gopherSession :: Socket -> IO ()
gopherSession conn = do
  path <- T.strip <$> T.pack <$> B.unpack <$> recv conn 1024
  valid <- isValidPath "." $ T.unpack path
  case valid of
    Just path -> do
      t <- fileType $ T.pack path
      case t of
        '1' -> do
          list <- getDirectory path
          formatted <- mapM formatFile list
          sendAll conn (B.pack $ T.unpack $ T.unlines formatted)
        _ -> do
          file <- B.pack <$> readFile path
          sendAll conn file
    Nothing -> return ()
