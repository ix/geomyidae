{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Gopher hiding (crlf)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= processArgs

processArgs :: [String] -> IO ()
processArgs [port] = run $ read port
processArgs _ = run 70

run :: Integer -> IO ()
run port = withSocketsDo $ do
  addr <- getAddrInfo (Just defaultHints) Nothing (Just $ show port)
  sock <- socket (addrFamily $ head addr) Stream defaultProtocol
  bind sock (addrAddress $ head addr)
  listen sock 1
  acceptLoop port sock
  close sock

acceptLoop :: Integer -> Socket -> IO ()
acceptLoop port sock = do
  (conn, _) <- accept sock
  gopherSession port conn
  close conn
  acceptLoop port sock

gopherSession :: Integer -> Socket -> IO ()
gopherSession port conn = do
  path <- T.strip <$> T.pack <$> B.unpack <$> recv conn 1024
  valid <- isValidPath "." $ T.unpack path
  case valid of
    Just path -> do
      t <- fileType $ T.pack path
      case t of
        '1' -> do
          list <- getDirectory path
          formatted <- mapM (\x -> formatFile x port) list
          sendAll conn (B.pack $ T.unpack $ T.unlines formatted)
        _ -> do
          file <- B.pack <$> readFile path
          sendAll conn file
    Nothing -> return ()
