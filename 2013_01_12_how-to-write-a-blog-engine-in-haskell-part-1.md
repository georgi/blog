How to write a blog engine in Haskell Part 1
============================================

I really like to evaluate programming languages based on their
practical value and one of the fun tasks is to write a small static
file blog engine. The engine just converts a bunch of markdown files
which are sorted into folders by month and year to html files given a
set of simple templates.

## Top level structure

Basically the rendering of blog posts can be done in three steps:

* Find all files that contain blog posts
* Read the markdown files into records
* Render each file to html and write to disk

In Haskell these steps could be written as:

```
writeBlog :: IO ()
writeBlog = do
  files <- findFiles "posts"
  posts <- mapM readPost files
  mapM_ writePost posts
```

When working inside the IO monad, you will use `mapM` and `mapM_`
quite often, similar to `map` they have following type signatures:

```
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
```

They apply a monadic operation `a -> m b` to each element of a list
and either you want to keep the result of each operation (mapM) or not
(mapM_).

Interestingly there is a more generalized version in `Data.Traversable`:

```
(Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
```

This function applies the monadic operation to anything that is
traversable. So you could build a tree data type and declare it as
instance of `Data.Traversable` and use this function to apply the
operation while maintaining the tree structure. For example a tree
of files would be converted into a tree of blog posts.

## Find files

So the first step in the blog rendering process would be to find all
files containing a post. So I created a folder named `posts` which
contains folders for each year, which in turn contain folders for each
month, which finally contain all the blog posts.

What we want now is the same as the UNIX command `find posts -type f`.

This can be done by looking at the entries of a folder and entering
each sub folder to examine its contents. When we find a file, we
collect it into a result list, which will contain all files
eventually.

When dealing with tree structures like a file system, recursion is an
elegant alternative to iteration. For each folder we find, the function
should call itself recursively until it ends up at the bottom most
level containing the files.

```
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
```

`findFiles` takes a `FilePath` and returns all files found in this
folder or its subfolders. When you pass a file it will just return a
list with this one file.

`getEntries` is a small helper function which returns all entries of
a folder without '.' and '..'.

The else branch works like this: it reads all entries of the folder,
prepends the current path, recurses for each entry, collects the
return values and flattens them into one big list.

## Reading posts into records

The data type representing a post is super simple. It just contains
the `folder` (e.g. "posts/2012/12"), the `file` (e.g. `post-title.md`)
and the `text` of the post.

```
data Post = Post { folder, file, text :: String };
```

`readPost` therefore takes a `FilePath` and reads the file contents,
decomposing the path name into `year`, `month` and `filename`, which we
feed into the record constructor.

```
readPost :: FilePath -> IO Post
readPost path = do
  s <- readFile path
  let [_, year, month, filename] = splitOn "/" path in
    return Post { folder = year ++ "/" ++ month, file = filename, text = s }
```

`splitOn` is taken from the `Data.List.Split` module in the `split` package
and splits a string into a list of strings given a delimiter string.

## To be continued

Next part in this series will take a look at the actual rendering of blog
post using the `sundown` package.
