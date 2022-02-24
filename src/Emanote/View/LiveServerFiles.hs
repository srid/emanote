module Emanote.View.LiveServerFiles
  ( tailwindFullCssPath,
    isLiveServerFile,
  )
where

import Data.Text qualified as T
import Relude

-- TODO: Check this compile-time using TH?

baseDir :: FilePath
baseDir = "_emanote-live-server"

tailwindFullCssPath :: FilePath
tailwindFullCssPath = baseDir <> "/tailwind/2.2.2/tailwind.min.css"

isLiveServerFile :: FilePath -> Bool
isLiveServerFile (toText -> fp) =
  toText baseDir `T.isPrefixOf` fp
