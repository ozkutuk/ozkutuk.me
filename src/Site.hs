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

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` dateField "dateMachine" "%Y-%m-%d"
    `mappend` defaultContext
