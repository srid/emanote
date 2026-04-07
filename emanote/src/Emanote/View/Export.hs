-- | Export an Emanote notebook to external formats.
module Emanote.View.Export (
  ExportFormat (..),
  renderExport,
) where

import Emanote.Model (Model)
import Emanote.Route.SiteRoute.Type (ExportFormat (..))
import Emanote.View.Export.Content
import Emanote.View.Export.JSON
import Relude

-- | Render the specified export format to LByteString
renderExport :: ExportFormat -> Model -> IO LByteString
renderExport exportFormat model =
  case exportFormat of
    ExportFormat_Metadata -> do
      pure $ renderJSONExport model
    ExportFormat_Content -> do
      content <- renderContentExport model
      pure $ encodeUtf8 content
