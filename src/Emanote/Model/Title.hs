{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Model.Title
  ( Title,

    -- * Title conversion
    fromRoute,
    fromInlines,
    toInlines,

    -- * Rendering a Title
    titleSplice,
  )
where

import Data.Aeson
import qualified Emanote.Route as R
import qualified Heist.Extra.Splices.Pandoc as HP
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

data Title
  = TitlePlain Text
  | TitlePandoc [B.Inline]
  deriving (Show, Ord, Generic, ToJSON)

instance Eq Title where
  (==) = on (==) toInlines

instance Semigroup Title where
  TitlePlain a <> TitlePlain b =
    TitlePlain (a <> b)
  x <> y =
    TitlePandoc $ on (<>) toInlines x y

instance IsString Title where
  fromString = TitlePlain . toText

fromRoute :: R.LMLRoute -> Title
fromRoute =
  TitlePlain . R.routeBaseName . R.lmlRouteCase

fromInlines :: [B.Inline] -> Title
fromInlines = TitlePandoc

toInlines :: Title -> [B.Inline]
toInlines = \case
  TitlePlain s -> one (B.Str s)
  TitlePandoc is -> is

titleSplice :: Monad n => Title -> HI.Splice n
titleSplice title =
  let titleDoc = B.Pandoc mempty $ one $ B.Plain $ toInlines title
   in HP.pandocSplice mempty (const . const $ Nothing) (const . const $ Nothing) titleDoc
