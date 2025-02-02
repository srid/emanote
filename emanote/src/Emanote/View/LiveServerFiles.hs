module Emanote.View.LiveServerFiles (
  isLiveServerFile,
  tailwindFullJsPath,
  tailwindJsFile,
)
where

import Data.Text qualified as T
import Emanote.Model.StaticFile (StaticFile)
import Emanote.Model.Type qualified as M
import Relude

-- TODO: Check this compile-time using TH?

baseDir :: FilePath
baseDir = "_emanote-live-server"

tailwindFullJsPath :: FilePath
tailwindFullJsPath = baseDir <> "/tailwind/tailwind.cdn.js"

isLiveServerFile :: FilePath -> Bool
isLiveServerFile (toText -> fp) =
  toText baseDir `T.isPrefixOf` fp

tailwindJsFile :: M.Model -> StaticFile
tailwindJsFile model =
  fromMaybe (error "model not ready?") $ M.modelLookupStaticFile tailwindFullJsPath model
