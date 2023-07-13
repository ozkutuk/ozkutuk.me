--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Text.Pandoc.Options (
  HTMLMathMethod (..),
  WriterOptions (..),
 )

--------------------------------------------------------------------------------

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
  match ("assets/*" .||. "CNAME") $ do
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

  create ["atom.xml"] $ do
    route idRoute
    compile (feedCompiler renderAtom)

  create ["rss.xml"] $ do
    route idRoute
    compile (feedCompiler renderRss)

  create ["feed.json"] $ do
    route idRoute
    compile (feedCompiler renderJson)

  match "templates/*" $ compile templateCompiler

type FeedRenderer = FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer = do
  let feedCtx = postCtx `mappend` bodyField "description"
  posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
  renderer feed feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "dateMachine" "%Y-%m-%d"
    `mappend` defaultContext
