module Emanote.View.LiveServerFiles
  ( tailwindFullCssUrl,
    isLiveServerFile,
  )
where

import qualified Data.Text as T

-- TODO: Check this compile-time using TH?

baseDir :: FilePath
baseDir = "_emanote-live-server"

tailwindFullCssUrl :: Text
tailwindFullCssUrl = toText baseDir <> "/tailwind/2.2.2/tailwind.min.css"

isLiveServerFile :: FilePath -> Bool
isLiveServerFile (toText -> fp) =
  toText baseDir `T.isPrefixOf` fp