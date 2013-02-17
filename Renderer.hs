{-# LANGUAGE OverloadedStrings #-}

module Renderer where

import Prelude hiding (div,id)
import Text.Atom.Feed
import Text.Atom.Feed.Export
import Text.Blaze.Html4.Strict hiding (head, map, title, contents)
import Text.Blaze.Html4.Strict.Attributes hiding (content, title)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html.Renderer.Pretty as H
import Text.XML.Light.Output
import Data.Time.Clock

import Types

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

atomFeed :: Blog -> [Post] -> UTCTime -> String
atomFeed blog posts time = ppElement $ xmlFeed $ blogFeed blog posts time

renderLayout :: Blog -> Html -> Html
renderLayout blog content = do
  docType
  html $ do
    H.head $ do
      H.title $ toHtml $ blogTitle blog
      meta ! httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
      link ! rel "alternate" ! type_ "application/atom+xml" ! href "/atom.xml" ! A.title "Atom 1.0"
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