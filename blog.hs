{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (div, id)
import Text.Atom.Feed
import Text.Atom.Feed.Export
import Text.Blaze.Html4.Strict hiding (head, map, title, contents)
import Text.Blaze.Html4.Strict.Attributes hiding (content, title)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Pretty as H
import Text.Sundown.Html.String as S
import Text.XML.Light.Output
import System.Directory
import System.Locale
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar

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
postLink post = "/" ++ (postFolder post) ++ "/" ++ (postName post) ++ ".html"

postDate :: Post -> String
postDate post = pubDate date where
  [year, month] = splitOn "/" (postFolder post)
  date = fromGregorian (read year) (read month) 1

postBody :: Post -> String
postBody post = S.renderHtml s allExtensions noHtmlModes True Nothing
  where s = concat $ intersperse "\n" $ drop 3 $ lines $ postText post

atomLink :: Blog -> Post -> Link
atomLink blog post = Link {
  linkHref = blogUri blog ++ postLink post,
  linkHrefLang = Nothing,
  linkRel = Nothing,
  linkType = Just "text/html",
  linkTitle = Nothing,
  linkLength = Nothing,
  linkAttrs = [],
  linkOther = []
};

postEntry :: Blog -> Post -> Entry
postEntry blog post = Entry {
  entryId = (blogUri blog) ++ (postLink post),
  entryTitle = TextString (postTitle post),
  entryUpdated = postDate post,
  entryAuthors = [],
  entryCategories = [],
  entryContent = Just $ HTMLContent $ postBody post,
  entryPublished = Just $ postDate post,
  entryContributor = [],
  entryLinks = [atomLink blog post],
  entryRights = Nothing,
  entrySource = Nothing,
  entrySummary = Nothing,
  entryInReplyTo = Nothing,
  entryInReplyTotal = Nothing,
  entryAttrs = [],
  entryOther = []
  }

blogFeed :: Blog -> [Post] -> UTCTime -> Feed
blogFeed blog posts time = Feed {
  feedId = blogUri blog,
  feedTitle = TextString $ blogTitle blog,
  feedUpdated = pubDate $ utctDay time,
  feedAuthors = [],
  feedCategories = [],
  feedContributors = [],
  feedGenerator = Nothing,
  feedLinks = [],
  feedIcon = Nothing,
  feedRights = Nothing,
  feedLogo = Nothing,
  feedSubtitle = Nothing,
  feedEntries = map (postEntry blog) posts,
  feedAttrs = [],
  feedOther = []
  }

readPost :: FilePath -> IO Post
readPost path = do
  s <- readFile path
  let [_, year, month, filename] = splitOn "/" path in
    return Post {
      postFolder = year ++ "/" ++ month,
      postFile = filename,
      postText = s
    }

renderLayout :: Blog -> Html -> Html
renderLayout blog content = do
  docType
  html $ do
    H.head $ do
      H.title $ toHtml $ blogTitle blog
      meta ! httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
      link ! rel "alternate" ! type_ "application/atom+xml" ! href "http://feeds.feedburner.com/matthias-georgi" ! A.title "Atom 1.0"
      link ! rel "stylesheet" ! media "screen" ! type_ "text/css" ! href "/style.css"
      link ! rel "stylesheet" ! href "/zenburn.css"
      script ! src "/highlight.pack.js" $ ""
      script $ "hljs.initHighlightingOnLoad();"
    body $ do
      div ! class_ "container" $ do
        h2 ! id "header" $ do
          a ! href "/" $ toHtml $ blogTitle blog
        div ! class_ "content" $ do
          preEscapedToHtml content

renderPost :: Post -> Html
renderPost post =
  div ! class_ "article" $ do
    h1 $ do
      a ! href (toValue (postLink post)) $ toHtml $ postTitle post
    preEscapedToHtml $ postBody post

renderPostPage :: Blog -> Post -> String
renderPostPage blog post = H.renderHtml $ renderLayout blog $ renderPost post

renderPostList :: Blog -> [Post] -> String -> String
renderPostList blog posts next = H.renderHtml $ do
  renderLayout blog $ do
    sequence_ $ map renderPost posts
    a ! rel "next" ! href (toValue next) $ "Next entries"

writePost :: Blog -> Post -> IO ()
writePost blog post = do
  createDirectoryIfMissing True ("public/" ++ (postFolder post))
  writeFile ("public/" ++ (postLink post)) $ renderPostPage blog post

fileForPage :: Int -> String
fileForPage 0 = ""
fileForPage 1 = "index.html"
fileForPage page = "page-" ++ show page ++ ".html"

writeIndex :: Blog -> Int -> Int -> [Post] -> IO ()
writeIndex _ _ _ [] = return ()
writeIndex blog page perPage posts = do
  writeFile file $ renderPostList blog posts' nextPage
  writeIndex blog (page + 1) perPage $ drop perPage posts
  where file = ("public/" ++ (fileForPage page))
        posts' = take perPage posts
        nextPage = fileForPage $ page + 1

writeFeed :: Blog -> [Post] -> IO ()
writeFeed blog posts = do
  time <- getCurrentTime
  writeFile "public/atom.xml" $ ppElement $ xmlFeed $ blogFeed blog posts time

writeBlog :: IO ()
writeBlog = do
  files <- findFiles "posts"
  posts <- mapM readPost files
  mapM_ (writePost blog) posts
  writeIndex blog 1 3 $ reverse posts
  writeFeed blog $ take 10 $ reverse posts
  where blog = Blog {
    blogUri = "http://www.matthias-georgi.de",
    blogTitle = "Matthias Georgi"
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

main :: IO ()
main = writeBlog
