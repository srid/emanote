module Emanote.View.LiveServerFiles (
  isLiveServerFile,
  tailwindCssFile,
) where

import Data.Text qualified as T
import Emanote.Model.StaticFile (StaticFile)
import Emanote.Model.Type qualified as M
import Relude

-- TODO: Check this compile-time using TH?

baseDir :: FilePath
baseDir = "_emanote-live-server"

tailwindFullCssPath :: FilePath
tailwindFullCssPath = baseDir <> "/tailwind/2.2.2/tailwind.min.css"

isLiveServerFile :: FilePath -> Bool
isLiveServerFile (toText -> fp) =
  toText baseDir `T.isPrefixOf` fp

tailwindCssFile :: M.Model -> StaticFile
tailwindCssFile model =
  fromMaybe (error "model not ready?") $ M.modelLookupStaticFile tailwindFullCssPath model
