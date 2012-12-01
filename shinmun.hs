import Text.StringTemplate
import Text.Sundown.Html.String
import System.Directory
import System.Locale
import Data.List.Split
import Data.Time.Format
import Data.Time.Calendar

data Post = Post { folder, file, text :: String };

postTitle :: Post -> String
postTitle post = head $ lines $ text post

postName :: Post -> String
postName  post = head $ splitOn "." $ file post

postLink :: Post -> String
postLink post = (folder post) ++ "/" ++ (postName post) ++ ".html"

postDate :: Post -> String
postDate post =
  let [year, month] = splitOn "/" (folder post)
      date = fromGregorian (read year) (read month) 1 in
    formatTime defaultTimeLocale "%d %b %y 00:00" date

postHtml :: Post -> String
postHtml  post = renderHtml (text post) allExtensions noHtmlModes True Nothing

postBody :: Post -> [String]
postBody  post = drop 3 (lines (text post))

postDesc :: Post -> String
postDesc  post = concat $ takeWhile (/= "") (postBody post)

readPost :: FilePath -> IO Post
readPost path = do
  s <- readFile path
  let [_, year, month, filename] = splitOn "/" path in
    return Post { folder = year ++ "/" ++ month, file = filename, text = s }

renderTmpl :: String -> [(String, String)] -> IO String
renderTmpl filename variables = do
  templateText <- readFile filename
  let template = newSTMP templateText in
    return $ toString $ setManyAttrib variables template

renderLayout :: String -> String -> IO String
renderLayout content root = do
  renderTmpl "templates/layout.html" [("content", content), ("root", root)]

postAttr :: Post -> [(String, String)]
postAttr post = [
  ("title", postTitle post),
  ("link", postLink post),
  ("date", postDate post),
  ("description", postDesc post)]

writePost :: Post -> IO ()
writePost post = do
  createDirectoryIfMissing True ("public/" ++ (folder post))
  html <- renderLayout (postHtml post) "../../"
  writeFile ("public/" ++ (postLink post)) html

writeIndex :: [Post] -> IO ()
writeIndex posts = do
  summaries <- mapM ((renderTmpl "templates/post.html") . postAttr) posts
  html <- renderLayout (concat summaries) ""
  writeFile "public/index.html" html

writeIndexRss :: [Post] -> IO ()
writeIndexRss posts = do
  items <- mapM ((renderTmpl "templates/item.xml") . postAttr) posts
  xml <- renderTmpl "templates/index.xml" [("content", concat items), ("time", "")]
  writeFile "public/index.rss" xml

writeBlog :: IO ()
writeBlog = do
  files <- findFiles "posts"
  posts <- mapM readPost files
  mapM_ writePost posts
  writeIndex $ reverse posts
  writeIndexRss $ reverse posts

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
