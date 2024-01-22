{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (for_)
import Hakyll
import Text.Pandoc.Options (
  HTMLMathMethod (..),
  WriterOptions (..),
 )

writerWithMath :: WriterOptions
writerWithMath =
  defaultHakyllWriterOptions {writerHTMLMathMethod = MathJax ""}

feed :: FeedConfiguration
feed =
  FeedConfiguration
    { feedTitle = "ozkutuk's blog"
    , feedDescription = "ozkutuk's blog"
    , feedAuthorName = "Berk Özkütük"
    , feedAuthorEmail = ""
    , feedRoot = "https://ozkutuk.me"
    }

config :: Configuration
config = defaultConfiguration {providerDirectory = "content"}

main :: IO ()
main = hakyllWith config $ do
  match ("assets/*" .||. "CNAME" .||. "*.asc") $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompilerWith defaultHakyllReaderOptions writerWithMath
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "about.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Home"
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  createFeeds

  match "templates/*" $ compile templateCompiler

type FeedRenderer = FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)

createFeeds :: Rules ()
createFeeds = for_ feedRoutes $ \(path, feed') ->
  create [path] $ do
    route idRoute
    compile (feedCompiler feed')
  where
    feedRoutes :: [(Identifier, FeedRenderer)]
    feedRoutes =
      [ ("feed.json", renderJson)
      , ("atom.xml", renderAtom)
      , ("rss.xml", renderRss)
      ]

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer = do
  let feedCtx = postCtx `mappend` bodyField "description"
  posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
  renderer feed feedCtx posts

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "dateMachine" "%Y-%m-%d"
    `mappend` defaultContext
