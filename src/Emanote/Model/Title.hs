{-# LANGUAGE DeriveAnyClass #-}

module Emanote.Model.Title
  ( Title,

    -- * Title conversion
    fromRoute,
    fromInlines,
    toInlines,

    -- * Rendering a Title
    titleSplice,
    titleSpliceNoHtml,
    toPlain,
  )
where

import Data.Aeson (ToJSON)
import Emanote.Pandoc.Markdown.Syntax.WikiLink (plainify)
import Emanote.Route qualified as R
import Heist.Extra.Splices.Pandoc qualified as HP
import Heist.Interpreted qualified as HI
import Relude
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

data Title
  = TitlePlain Text
  | TitlePandoc [B.Inline]
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Eq Title where
  (==) =
    -- Use toPlain here, rather than toInlines, because the same text can have
    -- different inlines structure. For example, "Foo Bar" can be represented as
    --   [Str "Foo", Space, Str "Bar"],
    -- or as,
    --   [Str "Foo Bar"]
    on (==) toPlain

instance Ord Title where
  compare =
    on compare toPlain

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

toPlain :: Title -> Text
toPlain = \case
  TitlePlain s -> s
  TitlePandoc is -> plainify is

titleSplice ::
  forall b.
  (W.Walkable B.Inline b, b ~ [B.Inline]) =>
  HP.RenderCtx ->
  (b -> b) ->
  Title ->
  HI.Splice Identity
titleSplice ctx f = \case
  TitlePlain x ->
    HI.textSplice x
  TitlePandoc is -> do
    let titleDoc = B.Pandoc mempty $ one $ B.Plain $ f is
    HP.pandocSplice ctx titleDoc

titleSpliceNoHtml :: Title -> HI.Splice Identity
titleSpliceNoHtml =
  HI.textSplice . toPlain
