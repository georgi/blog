How to write a blog engine in Haskell Part 2
============================================

In [my last post][1] layed out the top level structure of the blog
engine, how to find the right files from the posts folder and how to
represent posts as data types.

This time I'd like to introduce a type-safe way to render HTML
directly in Haskell without sacrificing readability or ease of use.

There are probably a million ways to render HTML but in practice there
are only a handful of possibilities you might consider for putting
data into HTML code.

## Template Languages

The most popular way of rendering HTML are good old PHP-style
templates, which let you interleave HTML code with executable bits of
a dumb language to fill in dynamic values. This is a straight-forward
approach which gets the job done but certainly has some
disadvantages:

* You might want to validate your HTML during development.  As the
template language itself is not a subset of HTML you can't vaidate the
template itself so easily.

* Reusable code lives as helper function which just returns dumb strings
instead of data structures leading to error-prone code.

* In general no type checking which makes it hard to build
abstractions on top of some rendering logic.

## Blaze Html

Let me introduce the [BlazeHtml][2] HTML combinator library for
Haskell. It's incredible simple to use, always generates valid HTML,
offers type-safety and is blazingly fast as the name already states.

Have a look at the type signature of a typical HTML combinator:

```
a :: Html -> Html
```

This function takes an `Html` element as content for a tag and returns
another `Html` element, which represents a link in this case.

A more complete example:

```
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

renderPage = docTypeHtml $ do
  body $ do
    ul $ forM_ [1 .. 10] (li . toHtml)
```

`renderPage` generates HTML for a list with numbers from 1 to 10.
Note that you don't have any impedance mismatch between HTML and
the host language. Code and data is easily mixed without losing
type-safety. This is a big deal for me. Just imagine all the time
you lost while reloading a web page after some minor editing just
to see you have to switch back to your editor again.

## Rendering Posts with Sundown

Rendering of posts should be simple and straighforward as possible.
A blog post is just a regular markdown file with a title as first
line. So to convert a blog post into html we basically just convert
the file via [Sundown][4], the markdown library from Github, and
insert the resulting HTML into a layout template.

```
{-# LANGUAGE OverloadedStrings #-}
import Text.Blaze.Html4.Strict hiding (head, map, title, contents)
import Text.Blaze.Html4.Strict.Attributes hiding (content, title)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Sundown.Html.String as S
import Data.List.Split

data Blog = Blog {
  blogUri,
  blogTitle :: String
};

data Post = Post {
  postFolder,
  postFile,
  postText:: String
};

-- Take the first line of a post file as post title.
postTitle :: Post -> String
postTitle post = head $ lines $ postText post

-- Return the filename of the post without extension.
postName :: Post -> String
postName post = head $ splitOn "." $ postFile post

-- Returns the path to the post on the website.
postLink :: Post -> String
postLink post = "/" ++ (postFolder post) ++ "/" ++ (postName post) ++ ".html"

-- Returns just the rendered body of a post without title.
postBody :: Post -> String
postBody post = S.renderHtml s allExtensions noHtmlModes True Nothing
  where s = concat $ intersperse "\n" $ drop 3 $ lines $ postText post

-- Render the html layout, insert the blog title, post title and post content.
renderLayout :: Blog -> Html -> Html
renderLayout blog content = do
  docType
  html $ do
    H.head $ do
      H.title $ toHtml $ blogTitle blog
    body $ do
      h2 ! id "header" $ do
        a ! href "/" $ toHtml $ blogTitle blog
      div ! class_ "content" $ do
        preEscapedToHtml content

-- Render a single post.
renderPost :: Post -> Html
renderPost post =
  div ! class_ "article" $ do
    h1 $ do
      a ! href (toValue (postLink post)) $ toHtml $ postTitle post
    preEscapedToHtml $ postBody post

-- Render a complete page containing one post.
renderPostPage :: Blog -> Post -> String
renderPostPage blog post = H.renderHtml $ renderLayout blog $ renderPost post
```

BlazeHtml escapes any value by default to prevent XSS. So any value
you want to insert has to be of type `Html` or `AttributeValue`. Look
at the code for the post title inside `renderPost`. The href for the link
needs to be converted and the text of the link as well.

`preEscapedToHtml` is an explicit way to insert raw strings into the HTML
document. In our case it is used to insert the page content into the 
layout and to insert the rendered markdown into the post template.

## Atom and RSS feeds

Next post we will have a look at rendering feeds with the [feed][5]
package. 

[1]: how-to-write-a-blog-engine-in-haskell-part-1.html
[2]: http://jaspervdj.be/blaze/
[3]: http://www.haskell.org/haskellwiki/Combinator_pattern
[4]: http://hackage.haskell.org/package/sundown 
[5]: http://hackage.haskell.org/package/feed-0.3.8
