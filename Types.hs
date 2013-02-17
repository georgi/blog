module Types where

import Data.Time.Format
import Data.Time.Calendar
import Data.List
import Data.List.Split
import System.Locale
import Network.AWS.AWSConnection
import Text.Sundown.Html.String as S

type Bucket = String
type AWS = (AWSConnection, Bucket)
type Page = (Int, Int)

data Blog = Blog {
  blogUri,
  blogTitle :: String
};

data Post = Post {
  postFolder,
  postFile,
  postText:: String
};

pubDate :: Day -> String
pubDate date = formatTime defaultTimeLocale "%d %b %y 00:00" date

postTitle :: Post -> String
postTitle post = head $ lines $ postText post

postName :: Post -> String
postName post = head $ splitOn "." $ postFile post

postLink :: Post -> String
postLink post = (postFolder post) ++ "/" ++ (postName post) ++ ".html"

postDate :: Post -> String
postDate post = pubDate date where
  [year, month] = splitOn "/" (postFolder post)
  date = fromGregorian (read year) (read month) 1

postBody :: Post -> String
postBody post = S.renderHtml s allExtensions noHtmlModes True Nothing
  where s = concat $ intersperse "\n" $ drop 3 $ lines $ postText post