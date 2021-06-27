{-# LANGUAGE RecordWildCards #-}

module Emanote.Pandoc.Filter.Embed where

import Control.Lens.Operators ((^.))
import Data.Map.Syntax ((##))
import qualified Data.Text as T
import qualified Ema.CLI
import Emanote.Model (Model)
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.StaticFile as SF
import qualified Emanote.Model.Title as Tit
import Emanote.Pandoc.Filter.Builtin (prepareNoteDoc)
import qualified Emanote.Pandoc.Filter.Query as PF
import qualified Emanote.Pandoc.Filter.Url as Url
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import qualified Heist.Extra as HE
import qualified Heist.Extra.Splices.Pandoc as HP
import qualified Heist.Extra.Splices.Pandoc as Splices
import Heist.Extra.Splices.Pandoc.Ctx (RenderCtx (..), ctxSansCustomSplicing)
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
          embedSiteRoute emaAction model ctx wl res
    _ ->
      Nothing
  where
    brokenLinkDivWrapper err block =
      HP.rpBlock ctx $
        B.Div (Url.brokenLinkAttr err) $
          one block

embedSiteRoute :: Monad n => Ema.CLI.Action -> Model -> HP.RenderCtx n -> WL.WikiLink -> Either MN.Note SF.StaticFile -> Maybe (HI.Splice n)
embedSiteRoute emaAction model RenderCtx {..} wl = \case
  Left note -> do
    pure . runEmbedTemplate "note" $ do
      "ema:note:title" ## Tit.titleSplice (MN._noteTitle note)
      "ema:note:url" ## HI.textSplice (SR.siteRouteUrl model $ SR.lmlSiteRoute $ note ^. MN.noteRoute)
      "ema:note:pandoc"
        -- TODO: DRY (see Template.hs use)
        ## Splices.pandocSplice
          classMap
          ( \ctx blk ->
              embedWikiLinkResolvingSplice emaAction model ctx blk
                <|> PF.queryResolvingSplice note model ctx blk
          )
          (Url.urlResolvingSplice emaAction model)
        $ note ^. MN.noteDoc & prepareNoteDoc model
  Right staticFile -> do
    let r = staticFile ^. SF.staticFileRoute
        fp = staticFile ^. SF.staticFilePath
    if
        | any (`T.isSuffixOf` toText fp) imageExts ->
          pure . runEmbedTemplate "image" $ do
            "ema:url" ## HI.textSplice (toText $ R.encodeRoute r)
            "ema:alt" ## HI.textSplice $ show wl
        | any (`T.isSuffixOf` toText fp) videoExts -> do
          pure . runEmbedTemplate "video" $ do
            "ema:url" ## HI.textSplice (toText $ R.encodeRoute r)
        | otherwise -> Nothing
  where
    runEmbedTemplate name splices = do
      tpl <- HE.lookupHtmlTemplateMust $ "/templates/filters/embed-" <> name
      HE.runCustomTemplate tpl splices

imageExts :: [Text]
imageExts =
  [ ".jpg",
    ".jpeg",
    ".png",
    ".svg",
    ".gif",
    ".bmp"
  ]

videoExts :: [Text]
videoExts =
  [ ".mp4",
    ".webm",
    ".ogv"
  ]
