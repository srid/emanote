{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Title where

import Data.Aeson
import qualified Emanote.Route as R
import qualified Heist.Extra.Splices.Pandoc as HP
import Heist.Extra.Splices.Pandoc.Render (plainify)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

data Title
  = TitlePlain Text
  | TitlePandoc [B.Inline]
  deriving (Eq, Show, Ord, Generic, ToJSON)

instance Semigroup Title where
  TitlePlain a <> TitlePlain b =
    TitlePlain (a <> b)
  TitlePandoc a <> TitlePandoc b =
    TitlePandoc (a <> b)
  TitlePlain a <> TitlePandoc b =
    TitlePandoc ([B.Str a] <> b)
  TitlePandoc a <> TitlePlain b =
    TitlePandoc (a <> [B.Str b])

fromRoute :: R.LMLRoute -> Title
fromRoute =
  TitlePlain . R.routeBaseName . R.lmlRouteCase

fromPlainAsPandoc :: Text -> Title
fromPlainAsPandoc = TitlePandoc . one . B.Str

fromPlain :: Text -> Title
fromPlain = TitlePlain

toPlain :: Title -> Text
toPlain = \case
  TitlePlain s -> s
  TitlePandoc is -> plainify is

toInlines :: Title -> [B.Inline]
toInlines = \case
  TitlePlain s -> one (B.Str s)
  TitlePandoc is -> is

titleSplice :: Monad n => Title -> HI.Splice n
titleSplice title =
  let titleDoc = B.Pandoc mempty $ one $ B.Plain $ toInlines title
   in HP.pandocSplice mempty (const . const $ Nothing) (const . const $ Nothing) titleDoc
