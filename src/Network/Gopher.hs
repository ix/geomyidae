{-# LANGUAGE OverloadedStrings #-}
module Network.Gopher where

import Prelude hiding (readFile, reverse, takeWhile)
import System.Directory
import System.FilePath
import Data.Text hiding (map)
import qualified Data.List as L (isPrefixOf)

crlf :: Text
crlf = "\r\n"

tab :: Text
tab = "\t"

fileExt :: Text -> Text
fileExt = reverse . takeWhile (/= '.') . reverse

isValidPath :: FilePath -> FilePath -> IO (Maybe FilePath)
isValidPath root path = do
  cRoot <- canonicalize root
  cPath <- canonicalize (root </> path)
  return $ if L.isPrefixOf cRoot cPath then
    Just cPath
  else Nothing
  where canonicalize = canonicalizePath

isImage :: Text -> Bool
isImage "jpg"  = True
isImage "jpeg" = True
isImage "png"  = True
isImage _      = False

fileType' :: Text -> Char
fileType' file
  | isImage ext   = 'I'
  | ext == "html" = 'h'
  | ext == "gif"  = 'g'
  | ext == "txt"  = '0'
  | otherwise     = '9'
  where ext = fileExt file

fileType :: Text -> IO Char
fileType filename = do
  isDir <- doesDirectoryExist $ unpack filename
  return (if isDir then '1' else fileType' filename)

formatFile :: Text -> Integer -> IO Text
formatFile filename port = do
  filetype <- fileType filename
  return ((t filetype) ++ filenameOnly ++ tab ++ filename ++ tab ++ "localhost" ++ tab ++ p)
  where
    filenameOnly = pack $ takeFileName $ unpack filename
    (++) = append
    t = pack . (:[])
    p = pack $ show port

getDirectory :: FilePath -> IO [Text]
getDirectory dir = do
  list <- listDirectory dir
  cList <- mapM (canonicalizePath . (dir </>)) list
  return $ map pack cList
