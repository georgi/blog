import Text.Pandoc 
import Text.StringTemplate
import System.IO
import System.Directory
import Text.Blaze.Renderer.String
import Data.List
import Data.List.Split

markdownToHtml :: String -> String
markdownToHtml = renderHtml . (writeHtml defaultWriterOptions) . (readMarkdown defaultParserState)

readPost :: String -> IO String
readPost path = do
  text <- readFile path
  return $ markdownToHtml text

renderTmpl :: String -> [(String, String)] -> IO String
renderTmpl file variables = do
  templateText <- readFile file
  let template = newSTMP templateText in
    return $ toString $ setManyAttrib variables template

renderLayout :: String -> String -> String -> IO ()
renderLayout target content root = do 
  html <- renderTmpl "templates/layout.html" [("content", content), ("root", root)]
  writeFile target html

renderPostSummary :: String -> IO String
renderPostSummary path = do 
  post <- readFile path
  let [prefix, year, month, file] = splitOn "/" path
      [basename, ext] = splitOn "." file
      title = head $ lines post
      link = year ++ "/" ++ month ++ "/" ++ basename ++ ".html" in
    renderTmpl "templates/post.html" [("title", title), ("link", link)]

writePost :: String -> IO ()
writePost path = do
  post <- readPost path
  let [prefix, year, month, file] = splitOn "/" path
      [basename, ext] = splitOn "." file
      dir = "public" ++ "/" ++ year ++ "/" ++ month
      target = dir ++ "/" ++ basename ++ ".html" in do
    createDirectoryIfMissing True dir
    renderLayout target post "../../"

writeIndex :: [String] -> IO ()
writeIndex posts = do
  summaries <- mapM renderPostSummary posts
  renderLayout "public/index.html" (concat summaries) ""

renderBlog :: IO ()
renderBlog = do
  posts <- findFiles "posts"
  mapM writePost posts
  writeIndex $ reverse posts
  

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
    getEntries path = do
      contents <- getDirectoryContents path
      return $ filter (\ f -> f /= "." && f /= "..") contents
