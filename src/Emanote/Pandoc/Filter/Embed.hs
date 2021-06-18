{-# LANGUAGE RecordWildCards #-}

module Emanote.Pandoc.Filter.Embed where

import Control.Lens.Operators ((^.))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Ema.CLI
import Emanote.Model (Model)
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.StaticFile as SF
import qualified Emanote.Pandoc.Filter.Query as PF
import qualified Emanote.Pandoc.Filter.Url as Url
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import qualified Emanote.Route as R
import qualified Heist.Extra.Splices.Pandoc as HP
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Ctx (RenderCtx (..), ctxSansCustomSplicing)
import Heist.Extra.Splices.Pandoc.Render (withoutH1)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B
import qualified Text.XmlHtml as X

embedWikiLinkResolvingSplice ::
  Monad n => Ema.CLI.Action -> Model -> HP.RenderCtx n -> B.Block -> Maybe (HI.Splice n)
embedWikiLinkResolvingSplice emaAction model (ctxSansCustomSplicing -> ctx) blk =
  case blk of
    B.Para [linkInline@(B.Link (_id, _class, otherAttrs) _is (url, tit))] -> do
      Rel.URTWikiLink (WL.WikiLinkEmbed, wl) <-
        Rel.parseUnresolvedRelTarget (otherAttrs <> one ("title", tit)) url
      case Url.resolveWikiLinkMustExist model wl of
        Left err ->
          pure $ brokenLinkDivWrapper err blk
        Right res -> do
          embedSiteRoute emaAction model ctx linkInline res
    _ ->
      Nothing
  where
    brokenLinkDivWrapper err block =
      HP.rpBlock ctx $
        B.Div (Url.brokenLinkAttr err) $
          one block

embedSiteRoute :: Monad n => Ema.CLI.Action -> Model -> HP.RenderCtx n -> B.Inline -> Either MN.Note SF.StaticFile -> Maybe (HI.Splice n)
embedSiteRoute emaAction model ctx@RenderCtx {..} linkInline = \case
  Left note -> do
    -- TODO: Add heading and box layout should be done via pandoc.tpl
    pure $ do
      embedNodes <-
        Splices.pandocSplice
          classMap
          (PF.queryResolvingSplice note model)
          (Url.urlResolvingSplice emaAction model)
          $ note ^. MN.noteDoc & withoutH1
      let embedBoxCls = ("class",) <$> Map.lookup "emanote:embed-box" classMap
          embedBoxHeaderCls = ("class",) <$> Map.lookup "emanote:embed-box:header" classMap
      linkHeading <-
        HP.rpInline (ctx {inlineSplice = Url.urlResolvingSplice emaAction model ctx}) linkInline
      pure $
        one $
          X.Element
            "div"
            (maybeToList embedBoxCls <> one ("title", "Embedded note"))
            $ X.Element "div" (maybeToList embedBoxHeaderCls) linkHeading :
            embedNodes
  Right staticFile -> do
    let r = staticFile ^. SF.staticFileRoute
        fp = staticFile ^. SF.staticFilePath
    HP.rpBlock ctx <$> do
      if any (`T.isSuffixOf` toText fp) imageExts
        then pure $ B.Plain $ one $ B.Image B.nullAttr [] (toText $ R.encodeRoute r, "")
        else Nothing

imageExts :: [Text]
imageExts =
  [ ".jpg",
    ".jpeg",
    ".png",
    ".svg",
    ".gif",
    ".bmp"
  ]
