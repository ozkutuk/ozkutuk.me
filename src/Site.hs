{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash (Digest, SHA1)
import Crypto.Hash qualified as Crypto
import Data.ByteArray qualified as ByteArray
import Data.ByteString.Base32.Z qualified as Z
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (for_)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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

  createFeeds

  createWkd "DAA0CE3407FB8F8C2B74DBB82759D1BC5A8659A8" "ozkutuk.me" "berk"

  match "templates/*" $ compile templateCompiler

createWkd :: String -> String -> String -> Rules ()
createWkd keyFile domain localPart = do
  createPolicy
  createKey
  where
    wkdWellknown = ".well-known/openpgpkey/"
    policy = wkdWellknown <> domain <> "/policy"
    keyPath = wkdWellknown <> domain <> "/hu/" <> hashLocalPart localPart

    createPolicy :: Rules ()
    createPolicy = create [fromFilePath policy] $ do
      route idRoute
      compile $ makeItem @String ""

    createKey :: Rules ()
    createKey = create [fromFilePath $ keyFile <> ".pgp"] $ do
      route $ constRoute keyPath
      compile copyFileCompiler

-- Extracted from: https://github.com/frasertweedale/hakyll-wkd/blob/master/site.hs
hashLocalPart :: String -> String
hashLocalPart localPart =
  let
    digest :: Digest SHA1
    digest = Crypto.hash . T.encodeUtf8 . T.toLower . T.pack $ localPart
   in
    BS8.unpack . Z.encode . ByteArray.convert $ digest

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
