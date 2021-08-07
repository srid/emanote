module Emanote.Model.Link.Resolve where

import Control.Lens.Operators ((^.))
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.WorldPeace.Union (openUnionLift)
import qualified Emanote.Model.Link.Rel as Rel
import qualified Emanote.Model.Note as MN
import qualified Emanote.Model.StaticFile as SF
import Emanote.Model.Type (Model)
import qualified Emanote.Model.Type as M
import qualified Emanote.Pandoc.Markdown.Syntax.WikiLink as WL
import qualified Emanote.Route as R
import qualified Emanote.Route.SiteRoute as SR

resolveUnresolvedRelTarget ::
  Model -> Rel.UnresolvedRelTarget -> Either Text (SR.SiteRoute, Maybe UTCTime)
resolveUnresolvedRelTarget model = \case
  Rel.URTWikiLink (_wlType, wl) -> do
    resourceSiteRoute <$> resolveWikiLinkMustExist model wl
  Rel.URTResource r ->
    resourceSiteRoute <$> resolveModelRouteMustExist r
  Rel.URTVirtual virtualRoute -> do
    pure $
      openUnionLift virtualRoute
        & (,Nothing)
  where
    resolveModelRouteMustExist r =
      case resolveModelRoute model r of
        Nothing ->
          Left "Link does not refer to any known file"
        Just v -> Right v

resolveWikiLinkMustExist :: MonadError Text m => Model -> WL.WikiLink -> m (Either MN.Note SF.StaticFile)
resolveWikiLinkMustExist model wl =
  case nonEmpty (M.modelWikiLinkTargets wl model) of
    Nothing -> do
      throwError "Wiki-link does not refer to any known file"
    Just (target :| []) ->
      pure target
    Just targets -> do
      let targetsStr =
            targets <&> \case
              Left note -> R.encodeRoute $ R.lmlRouteCase $ note ^. MN.noteRoute
              Right sf -> R.encodeRoute $ sf ^. SF.staticFileRoute
      throwError $
        "Wikilink "
          <> show wl
          <> " is ambiguous; referring to one of: "
          <> T.intercalate ", " (toText <$> toList targetsStr)

resolveModelRoute :: Model -> R.ModelRoute -> Maybe (Either MN.Note SF.StaticFile)
resolveModelRoute model lr = do
  let eRoute = R.modelRouteCase lr
  bitraverse
    (`M.modelLookupNoteByRoute` model)
    (`M.modelLookupStaticFileByRoute` model)
    eRoute

resourceSiteRoute :: Either MN.Note SF.StaticFile -> (SR.SiteRoute, Maybe UTCTime)
resourceSiteRoute = \case
  Left note ->
    SR.noteFileSiteRoute note
      & (,Nothing)
  Right sf ->
    SR.staticFileSiteRoute sf
      & (,Just $ sf ^. SF.staticFileTime)
