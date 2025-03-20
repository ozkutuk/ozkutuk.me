{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (filterM, (>=>))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Hakyll
import System.FilePath qualified as File
import System.IO (BufferMode (..), Handle, hSetBuffering)
import System.Process.Typed qualified as Process
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Highlighting qualified as Pandoc
import Text.Pandoc.Options (
  HTMLMathMethod (..),
  WriterOptions (..),
 )
import Text.Pandoc.Walk qualified as Pandoc
import qualified System.Environment as Env

pandocCodeStyle :: Pandoc.Style
pandocCodeStyle = Pandoc.kate

hakyllWriterOptions :: WriterOptions
hakyllWriterOptions =
  defaultHakyllWriterOptions
    { writerHTMLMathMethod = KaTeX ""
    , writerHighlightStyle = Just pandocCodeStyle
    }

blogRoot :: String
blogRoot = "https://ozkutuk.me"

feed :: FeedConfiguration
feed =
  FeedConfiguration
    { feedTitle = "ozkutuk's blog"
    , feedDescription = "ozkutuk's blog"
    , feedAuthorName = "Berk Özkütük"
    , feedAuthorEmail = ""
    , feedRoot = blogRoot
    }

config :: Configuration
config = defaultConfiguration {providerDirectory = "content"}

main :: IO ()
main = hakyllWith config $ do
  match
    ( "assets/*"
        .||. "CNAME"
        .||. "*.asc"
        .||. "css/fonts/**"
        .||. "robots.txt"
        .||. "favicon.png"
    )
    $ do
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
      pandocCompiler'
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postContext
        >>= loadAndApplyTemplate "templates/default.html" postContext
        >>= relativizeUrls

  matchMetadata "posts/*" isDraft $ do
    route . customRoute $ makeDraftUrl
    compile $
      pandocCompiler'
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postContext
        >>= loadAndApplyTemplate "templates/default.html" postContext
        >>= relativizeUrls

  match "about.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      publishedPosts <- recentFirst =<< filterPublished =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" context (pure publishedPosts)
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
  let feedCtx = context <> bodyField "description"
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

context :: Context String
context =
  dateField "date" "%B %e, %Y"
    <> dateField "dateMachine" "%Y-%m-%d"
    <> isDraftField
    <> constField "root" blogRoot
    <> constField "og-image" "https://www.libravatar.org/avatar/cb5ae1412b8866d55b8a957468696797?s=200"
    <> defaultContext

postContext :: Context String
postContext =
  openGraphField "opengraph" context
    <> context

-- Adapted from: https://tony-zorman.com/posts/katex-with-hakyll.html
hlKaTeX :: Pandoc.Pandoc -> Compiler Pandoc.Pandoc
hlKaTeX pandoc = recompilingUnsafeCompiler $ do
  env <- Env.getEnvironment
  let env' = ("DENO_DIR", "scripts/deno-cache") : env
  let script =
        Process.shell "deno run --cached-only scripts/math.ts"
          & Process.setStdin Process.createPipe
          & Process.setStdout Process.createPipe
          & Process.setEnv env'
  Process.withProcessTerm script $ \p -> do
    hSetBuffering (Process.getStdin p) NoBuffering
    hSetBuffering (Process.getStdout p) NoBuffering

    (`Pandoc.walkM` pandoc) $ \case
      Pandoc.Math mathType (T.unwords . T.lines . T.strip -> text) -> do
        let math :: Text = case mathType of
              Pandoc.DisplayMath -> ":DISPLAY " <> text
              Pandoc.InlineMath -> text

        T.hPutStrLn (Process.getStdin p) math
        Pandoc.RawInline "html" <$> getResponse (Process.getStdout p)
      block -> pure block
  where
    -- KaTeX might sent the input back as multiple lines if it involves a
    -- matrix of coordinates. The big assumption here is that it does so only
    -- when matrices—or other such constructs—are involved, and not when it
    -- sends back "normal" HTML.
    getResponse :: Handle -> IO Text
    getResponse handle = go ""
      where
        go :: Text -> IO Text
        go !str = do
          more <- (str <>) <$> T.hGetLine handle
          if ">" `T.isSuffixOf` more -- end of HTML snippet
            then pure more
            else go more

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = pandocCompilerWithTransformM defaultHakyllReaderOptions hakyllWriterOptions hlKaTeX
