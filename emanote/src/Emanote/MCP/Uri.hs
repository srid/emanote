{- | URI schema for the Emanote MCP resources.

External clients hard-code these URIs — changing them is a breaking
protocol change. The schema is currently unversioned; if a future
phase needs versioning, a prefix revision lands here.
-}
module Emanote.MCP.Uri (
  metadataUri,
  contentUri,
  noteUriPrefix,
  noteUriTemplate,
  uriToKind,
  kindToUri,
) where

import Data.Text qualified as T
import Emanote.MCP.Catalog (ResourceKind (..))
import Relude

metadataUri :: Text
metadataUri = "emanote://export/metadata"

contentUri :: Text
contentUri = "emanote://export/content"

noteUriPrefix :: Text
noteUriPrefix = "emanote://note/"

-- | RFC 6570 template for the per-note URI.
noteUriTemplate :: Text
noteUriTemplate = noteUriPrefix <> "{path}"

uriToKind :: Text -> Maybe ResourceKind
uriToKind uri
  | uri == metadataUri = Just MetadataJson
  | uri == contentUri = Just ContentMarkdown
  | Just path <- T.stripPrefix noteUriPrefix uri = Just (Note (toString path))
  | otherwise = Nothing

kindToUri :: ResourceKind -> Text
kindToUri = \case
  MetadataJson -> metadataUri
  ContentMarkdown -> contentUri
  Note path -> noteUriPrefix <> toText path
