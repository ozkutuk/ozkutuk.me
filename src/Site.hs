{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM, (>=>))
import Data.Foldable (for_)
import Hakyll
import System.FilePath qualified as File
import Text.Pandoc.Options (
  HTMLMathMethod (..),
  WriterOptions (..),
 )
import Text.Pandoc.Highlighting qualified as Pandoc

pandocCodeStyle :: Pandoc.Style
pandocCodeStyle = Pandoc.kate

hakyllWriterOptions :: WriterOptions
hakyllWriterOptions =
  defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    , writerHighlightStyle = Just pandocCodeStyle
    }

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
  match ("assets/*" .||. "CNAME" .||. "*.asc" .||. "css/fonts/**") $ do
    route idRoute
    compile copyFileCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ Pandoc.styleToCss pandocCodeStyle

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  matchMetadata "posts/*" isPublished $ do
    route $ setExtension "html"
    compile $
      pandocCompilerWith defaultHakyllReaderOptions hakyllWriterOptions
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  matchMetadata "posts/*" isDraft $ do
    route . customRoute $ makeDraftUrl
    compile $
      pandocCompilerWith defaultHakyllReaderOptions hakyllWriterOptions
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "about.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      publishedPosts <- recentFirst =<< filterPublished =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (pure publishedPosts)
              <> constField "title" "Home"
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  createFeeds

  match "templates/*" $ compile templateCompiler

makeDraftUrl :: Identifier -> FilePath
makeDraftUrl = File.combine "drafts" . flip File.replaceExtension "html" . File.takeFileName . toFilePath

isDraft :: Metadata -> Bool
isDraft = (Just "true" ==) . lookupString "draft"

isPublished :: Metadata -> Bool
isPublished = not . isDraft

filterPublished :: (MonadMetadata m) => [Item a] -> m [Item a]
filterPublished = filterM (fmap isPublished . getMetadata . itemIdentifier)

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
  let feedCtx = postCtx <> bodyField "description"
  posts <- fmap (take 10) . recentFirst =<< filterPublished =<< loadAllSnapshots "posts/*" "content"
  renderer feed feedCtx posts

-- Copied from Hakyll source since it is not exported
field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k _ i ->
  if k == key
    then value i
    else noResult $ "Tried field " ++ key

-- | Similar to 'boolField', but input function returns a 'Compiler'.
-- Useful for accessing metadata.
boolField' :: String -> (Item a -> Compiler Bool) -> Context a
boolField' name f =
  field' name $
    f >=> \case
      True -> pure EmptyField
      False -> noResult $ "Field " ++ name ++ " is false"

isDraftField :: Context String
isDraftField = boolField' "isDraft" (fmap isDraft . getMetadata . itemIdentifier)

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> dateField "dateMachine" "%Y-%m-%d"
    <> isDraftField
    <> defaultContext
