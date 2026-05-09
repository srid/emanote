{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.StaticFile where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Emanote.Route qualified as R
import Optics.TH (makeLenses)
import Relude
import Skylighting qualified
import System.FilePath (takeExtension)

data StaticFile = StaticFile
  { _staticFileRoute :: R.R 'R.AnyExt
  , _staticFilePath :: FilePath
  , _staticFileTime :: UTCTime
  -- ^ Indicates that this file was updated no latter than the given time.
  , _staticFileInfo :: Maybe StaticFileInfo
  -- ^ This file might have its content read
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

type StaticFileIxs = '[R.R 'R.AnyExt, WL.WikiLink]

type IxStaticFile = IxSet StaticFileIxs StaticFile

instance Indexable StaticFileIxs StaticFile where
  indices =
    ixList
      (ixFun $ one . _staticFileRoute)
      (ixFun $ toList . staticFileSelfRefs)

staticFileSelfRefs :: StaticFile -> NonEmpty WL.WikiLink
staticFileSelfRefs =
  fmap snd
    . WL.allowedWikiLinks
    . R.unRoute
    . _staticFileRoute

data StaticFileInfo where
  StaticFileInfoImage :: StaticFileInfo
  StaticFileInfoAudio :: StaticFileInfo
  StaticFileInfoVideo :: StaticFileInfo
  StaticFileInfoPDF :: StaticFileInfo
  StaticFileInfoCode ::
    -- | File code language name
    CodeLanguage ->
    -- | File content
    Text ->
    StaticFileInfo
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Aeson.ToJSON)

newtype CodeLanguage = CodeLanguage Text
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Aeson.ToJSON)

-- | Return the ${name} in the corresponding templates/filters/embed-${name}.tpl
staticFileInfoTemplateName :: (IsString s) => StaticFileInfo -> s
staticFileInfoTemplateName = \case
  StaticFileInfoImage -> "image"
  StaticFileInfoAudio -> "audio"
  StaticFileInfoVideo -> "video"
  StaticFileInfoPDF -> "pdf"
  StaticFileInfoCode _ _ -> "code"

readStaticFileInfo ::
  (Monad m) =>
  FilePath ->
  (FilePath -> m Text) ->
  m (Maybe StaticFileInfo)
readStaticFileInfo fp readFilePath = do
  let extension = takeExtension fp
  if
    | toText extension `elem` imageExts ->
        pure $ Just StaticFileInfoImage
    | toText extension `elem` videoExts ->
        pure $ Just StaticFileInfoVideo
    | toText extension `elem` audioExts ->
        pure $ Just StaticFileInfoAudio
    | extension == ".pdf" ->
        pure $ Just StaticFileInfoPDF
    | Just lang <- codeLanguageForExtension extension -> do
        code <- readFilePath fp
        pure $ Just $ StaticFileInfoCode lang code
    | otherwise -> return Nothing
  where
    imageExts = [".jpg", ".jpeg", ".png", ".svg", ".gif", ".bmp", ".webp"]
    videoExts = [".mp4", ".webm", ".ogv"]
    audioExts = [".aac", ".caf", ".flac", ".mp3", ".ogg", ".wav", ".wave"]

{- | Look up a source-code language for a file extension via skylighting's
own bundled syntax map.

Skylighting's @syntaxesByExtension@ globs the extension against the
@sExtensions@ patterns parsed from each Kate syntax XML, so this delegates
the entire ext→language mapping to the highlighter we render with. Adding
support for a new language is upstream work in skylighting, not a list to
edit here. The lower-cased short-name is used as the language identifier
(matches the @\<code class="sourceCode haskell"\>@ convention skylighting
itself emits).

Caveat: matching an extension causes the file's content to be read into
the model eagerly at build time, even if no note actually embeds it. This
is the same shape as the previous hand-curated map, just with broader
coverage.
-}
codeLanguageForExtension :: String -> Maybe CodeLanguage
codeLanguageForExtension extension =
  case Skylighting.syntaxesByExtension Skylighting.defaultSyntaxMap extension of
    syntax : _ -> Just $ CodeLanguage $ T.toLower $ Skylighting.sShortname syntax
    [] -> Nothing

makeLenses ''StaticFile
