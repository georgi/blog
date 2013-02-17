{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Spawn
import Data.Time.Clock
import Data.List
import Data.List.Split
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.Char8 (pack)
import Network.AWS.AWSConnection
import Network.AWS.S3Object

import Types
import Renderer

readPost :: FilePath -> IO Post
readPost path = do
  s <- readFile path
  let [_, year, month, filename] = splitOn "/" path in
    return Post {
      postFolder = year ++ "/" ++ month,
      postFile = filename,
      postText = s
    }

findFiles :: FilePath -> IO [FilePath]
findFiles path = do
  isFile <- doesFileExist path
  if isFile then
      return [path]
    else do
      entries <- getEntries path
      paths <- return $ map (\ entry -> path ++ "/" ++ entry) entries
      files <- mapM findFiles paths
      return $ concat files
  where
    getEntries filepath = do
      contents <- getDirectoryContents filepath
      return $ filter ((/= '.') . head) contents

contentTypeFor :: String -> String
contentTypeFor file = case last $ splitOn "." file of
  ('j':'s':[]) -> "text/javascript"
  ('c':'s':'s':[]) -> "text/css"
  ('x':'m':'l':[]) -> "text/xml"
  ('h':'t':'m':[]) -> "text/html"
  ('h':'t':'m':'l':[]) -> "text/html"
  _ -> "application/binary"

write :: AWS -> FilePath -> BS.ByteString -> IO ()
write (conn,bucket) path fileBody = do
  res <- sendObject conn obj
  case res of
    Left err -> do
      error $ show err
    Right _ -> do
      putStrLn path
  where obj = S3Object {
          obj_bucket = bucket,
          obj_name = path,
          obj_headers = [],
          content_type = contentTypeFor path,
          obj_data = fileBody }

writeFeed :: AWS -> Blog -> [Post] -> IO ()
writeFeed aws blog posts = do
  time <- getCurrentTime
  write aws "atom.xml" $ pack $ atomFeed blog posts time

writeIndex :: AWS -> Blog -> Page -> [Post] -> IO ()
writeIndex _ _ _ [] = return ()
writeIndex aws blog (page,pageSize) posts = do
  write aws (fileForPage page) $ pack $ renderPostList blog posts' nextPage
  writeIndex aws blog ((page + 1), pageSize) $ drop pageSize posts
  where posts' = take pageSize posts
        nextPage = fileForPage $ page + 1
        fileForPage 0 = ""
        fileForPage 1 = "index.html"
        fileForPage p = "page-" ++ show p ++ ".html"

writePost :: AWS -> Blog -> Post -> IO ()
writePost aws blog post = do
  write aws (postLink post) $ pack $ renderPostPage blog post

writePublicFile :: AWS -> FilePath -> IO ()
writePublicFile aws file = do
  BS.readFile file >>= write aws path
  where path = concat $ intersperse "/" $ drop 1 $ splitOn "/" file

writePublic :: AWS -> IO ()
writePublic aws = findFiles "public" >>= parMapIO_ (writePublicFile aws)

writeBlog :: AWS -> Blog -> IO ()
writeBlog aws blog = do
  posts <- findFiles "posts" >>= mapM readPost
  parMapIO_ (writePost aws blog) posts
  writeIndex aws blog (1,3) $ reverse posts
  writeFeed aws blog $ take 10 $ reverse posts

getBlog :: IO Blog
getBlog = do
  uri <- getEnv "BLOG_URI"
  title <- getEnv "BLOG_TITLE"
  return Blog {
    blogUri = uri,
    blogTitle = title
  }

getAWS :: IO AWS
getAWS = do
  c  <- amazonS3ConnectionFromEnv
  case c of
    Just connection -> do
      bucket <- getEnv "AWS_BUCKET"
      return (connection, bucket)
    Nothing -> do
      error "Please set AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY"

main :: IO ()
main = do
  aws <- getAWS
  blog <- getBlog
  writePublic aws
  writeBlog aws blog
