module Emanote.Pandoc.Filter.Embed where

import Control.Lens.Operators ((^.))
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
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

embedWikiLinkResolvingSplice ::
  Monad n => Ema.CLI.Action -> Model -> HP.RenderCtx n -> B.Block -> Maybe (HI.Splice n)
embedWikiLinkResolvingSplice emaAction model (ctxSansCustomSplicing -> ctx) blk =
  case blk of
    B.Para [B.Link (_id, _class, otherAttrs) _is (url, tit)] -> do
      Rel.URTWikiLink (WL.WikiLinkEmbed, wl) <-
        Rel.parseUnresolvedRelTarget (otherAttrs <> one ("title", tit)) url
      case Url.resolveWikiLinkMustExist model wl of
        Left err ->
          pure $ brokenLinkDivWrapper err blk
        Right res -> do
          embedSiteRoute emaAction model ctx res
    _ ->
      Nothing
  where
    brokenLinkDivWrapper err block =
      HP.rpBlock ctx $
        B.Div (Url.brokenLinkAttr err) $
          one block

embedSiteRoute :: Monad n => Ema.CLI.Action -> Model -> HP.RenderCtx n -> Either MN.Note SF.StaticFile -> Maybe (HI.Splice n)
embedSiteRoute emaAction model ctx = \case
  Left note -> do
    -- TODO: Add heading and box
    pure $
      Splices.pandocSplice
        mempty
        (PF.queryResolvingSplice note model)
        (Url.urlResolvingSplice emaAction model)
        $ note ^. MN.noteDoc
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
    ".svg"
  ]