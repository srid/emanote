module Emanote.Pandoc.Filter.Embed where

import Control.Lens.Operators ((^.))
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.WorldPeace.Union (absurdUnion, openUnionLift)
import qualified Ema
import qualified Ema.CLI
import Emanote.Model (Model)
import qualified Emanote.Model as M
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.StaticFile as SF
import qualified Emanote.Pandoc.Filter.Url as Url
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import Emanote.Prelude (h)
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR
import qualified Heist.Extra.Splices.Pandoc as HP
import Heist.Extra.Splices.Pandoc.Ctx (ctxSansCustomSplicing)
import Heist.Extra.Splices.Pandoc.Render (plainify)
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

embedWikiLinkResolvingSplice ::
  Monad n => Model -> HP.RenderCtx n -> B.Block -> Maybe (HI.Splice n)
embedWikiLinkResolvingSplice model (ctxSansCustomSplicing -> ctx) blk =
  case blk of
    B.Para [B.Link (_id, _class, otherAttrs) _is (url, tit)] -> do
      uRel@(Rel.URTWikiLink (WL.WikiLinkEmbed, _)) <-
        Rel.parseUnresolvedRelTarget (otherAttrs <> one ("title", tit)) url
      case Url.resolveUnresolvedRelTarget model uRel of
        Left err ->
          pure $ brokenLinkDivWrapper err blk
        Right (fst -> sr) -> do
          embedSiteRoute model ctx sr
    _ ->
      Nothing
  where
    brokenLinkDivWrapper err block =
      HP.rpBlock ctx $
        B.Div (Url.brokenLinkAttr err) $
          one block

embedSiteRoute :: Monad n => Model -> HP.RenderCtx n -> SR.SiteRoute -> Maybe (HI.Splice n)
embedSiteRoute model ctx =
  absurdUnion
    `h` (\(SR.MissingR _) -> Nothing)
    `h` ( \(resR :: SR.ResourceRoute) ->
            resR & absurdUnion
              `h` ( \(lmlR :: R.LMLRoute) ->
                      HP.rpBlock ctx <$> do
                        pure $ B.Plain [B.Str "emanote-error: ![[..]] is TODO; see https://github.com/srid/emanote/issues/24"]
                  )
              `h` ( \(r :: R.StaticFileRoute, fp :: FilePath) ->
                      HP.rpBlock ctx <$> do
                        if any (`T.isSuffixOf` toText fp) imageExts
                          then pure $ B.Plain $ one $ B.Image B.nullAttr [] (toText $ R.encodeRoute r, "")
                          else Nothing
                  )
        )
    `h` (\(_ :: SR.VirtualRoute) -> Nothing)

imageExts :: [Text]
imageExts =
  [ ".jpg",
    ".jpeg",
    ".png",
    ".svg"
  ]