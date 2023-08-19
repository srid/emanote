{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.StaticFile where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Emanote.Route qualified as R
import Optics.TH (makeLenses)
import Relude
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
  let extension = toText (takeExtension fp)
  if
      | extension `elem` imageExts ->
          pure $ Just StaticFileInfoImage
      | extension `elem` videoExts ->
          pure $ Just StaticFileInfoVideo
      | extension `elem` audioExts ->
          pure $ Just StaticFileInfoAudio
      | extension == ".pdf" ->
          pure $ Just StaticFileInfoPDF
      | Just lang <- Map.lookup extension codeExts -> do
          code <- readFilePath fp
          pure $ Just $ StaticFileInfoCode lang code
      | otherwise -> return Nothing
  where
    imageExts = [".jpg", ".jpeg", ".png", ".svg", ".gif", ".bmp", ".webp"]
    videoExts = [".mp4", ".webm", ".ogv"]
    audioExts = [".aac", ".caf", ".flac", ".mp3", ".ogg", ".wav", ".wave"]
    codeExts =
      CodeLanguage
        <$> Map.fromList
          [ (".hs", "haskell")
          , (".nix", "nix")
          , (".sh", "bash")
          , (".py", "python")
          , (".js", "javascript")
          , (".java", "java")
          , (".c", "c")
          , (".cpp", "cpp")
          , (".cs", "cs")
          , (".rb", "ruby")
          , (".go", "go")
          , (".swift", "swift")
          , (".kt", "kotlin")
          , (".rs", "rust")
          , (".ts", "typescript")
          , (".php", "php")
          ]

makeLenses ''StaticFile
