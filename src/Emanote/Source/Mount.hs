-- | This is a fork of Ema.Helper.FileSystem (forked for ghcid-convenience), to
-- be *soon* made a separate Haskell library.
module Emanote.Source.Mount where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, try)
import Control.Monad.Logger
  ( LogLevel (LevelDebug, LevelError, LevelInfo),
    MonadLogger,
    logWithoutLoc,
  )
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import System.Directory (canonicalizePath)
import System.FSNotify
  ( ActionPredicate,
    Event (..),
    StopListening,
    WatchManager,
    watchTree,
    withManager,
  )
import System.FilePath (isRelative, makeRelative)
import System.FilePattern (FilePattern, (?==))
import System.FilePattern.Directory (getDirectoryFilesIgnore)
import UnliftIO (MonadUnliftIO, toIO, withRunInIO)

-- | Mount the given directory on to the given LVar such that any filesystem
-- events (represented by `FileAction`) are made to be reflected in the LVar
-- model using the given model update function.
mountOnLVar ::
  forall model m b s.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLogger m,
    Show b,
    Ord b
  ) =>
  -- | The base directory of files to use as "fallback" if the corresponding path is missing (or gets removed) in the mounted directory.
  --
  -- NOTE: Auto reload is not supported on this directory (yet)
  Maybe (s, FilePath) ->
  -- | The directory to mount.
  (s, FilePath) ->
  -- | Only include these files (exclude everything else)
  [(b, FilePattern)] ->
  -- | Ignore these patterns
  [FilePattern] ->
  -- | The `LVar` onto which to mount.
  --
  -- NOTE: It must not be set already. Otherwise, the value will be overriden
  -- with the initial value argument (next).
  LVar model ->
  -- | Initial value of model, onto which to apply updates.
  model ->
  -- | How to update the model given a file action.
  --
  -- `b` is the tag associated with the `FilePattern` that selected this
  -- `FilePath`. `FileAction` is the operation performed on this path. This
  -- should return a function (in monadic context) that will update the model,
  -- to reflect the given `FileAction`.
  --
  -- The `FilePath` is relative to mounted directory, unless the fallback path
  -- is returned (or a symlink is found), which would be absolute.
  --
  -- If the action throws an exception, it will be logged and ignored.
  ([(b, [(s, (Maybe s, FilePath))])] -> FileAction -> m (model -> model)) ->
  m ()
mountOnLVar mFallbackFolder (stag, folder) pats ignore var var0 toAction' = do
  log LevelInfo $ "Mounting path " <> toText folder <> " (filter: " <> show pats <> ") onto LVar"
  let toAction x = interceptExceptions id . toAction' x
  -- NOTE: We read the fallback files only once. Auto reload is not supported, yet.
  (fbFiles, fallbacks, stagBase) <-
    case mFallbackFolder of
      Nothing ->
        pure (mempty, mempty, stag)
      Just (stagBase, fallbackFolder) -> do
        canonical <- liftIO $ canonicalizePath fallbackFolder
        log LevelInfo $ "Mount base: " <> toText canonical
        fbFiles <- maybe (pure mempty) (\(_, path) -> filesMatchingWithTag path pats ignore) mFallbackFolder
        let fallbacks :: Map FilePath (FilePath, b) =
              Map.fromList $
                flip concatMap fbFiles $ \(t, fs) ->
                  fs <&> \fp ->
                    (fp, (fp, t))
        pure (fbFiles, fallbacks, stagBase)
  let withBase f = (stag, (const (Just stagBase) =<< Map.lookup f fallbacks, f))
  LVar.set var =<< do
    fbFilesF <- toAction (second (fmap ((stagBase,) . (Nothing,))) <$> fbFiles) Update
    fs <- filesMatchingWithTag folder pats ignore
    fsF <- toAction (second (fmap withBase) <$> fs) Update
    pure $ fsF . fbFilesF $ var0
  onChange folder $ \fp change -> do
    -- TODO: Should refactor the ignore part to be integral to pats, and be part
    -- of `getTag`
    let shouldIgnore = any (?== fp) ignore
    whenJust (guard (not shouldIgnore) >> getTag pats fp) $ \tag -> do
      -- TODO: We should probably debounce and group frequently-firing events
      -- here, but do so such that `change` is the same for the events in the
      -- group.
      let restoreFallback = do
            guard $ change == Delete
            (fallbackFp, fallbackTag) <- Map.lookup fp fallbacks
            unless (fallbackTag == tag) $ error "Fallback tag is non-equivalent (impossible)"
            pure (stagBase, (Nothing, fallbackFp))
      let groupOfOne = one (tag, one $ fromMaybe (withBase fp) restoreFallback)
      action <- toAction groupOfOne change
      LVar.modify var action
  where
    -- Log and ignore exceptions
    --
    -- TODO: Make user defineeable?
    interceptExceptions :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => a -> m a -> m a
    interceptExceptions default_ f = do
      f' <- toIO f
      liftIO (try f') >>= \case
        Left (ex :: SomeException) -> do
          log LevelError $ "User exception: " <> show ex
          pure default_
        Right v ->
          pure v

filesMatching :: (MonadIO m, MonadLogger m) => FilePath -> [FilePattern] -> [FilePattern] -> m [FilePath]
filesMatching parent' pats ignore = do
  parent <- liftIO $ canonicalizePath parent'
  log LevelInfo $ toText $ "Traversing " <> parent <> " for files matching " <> show pats <> ", ignoring " <> show ignore
  liftIO $ getDirectoryFilesIgnore parent pats ignore

-- | Like `filesMatching` but with a tag associated with a pattern so as to be
-- able to tell which pattern a resulting filepath is associated with.
filesMatchingWithTag :: (MonadIO m, MonadLogger m, Ord b) => FilePath -> [(b, FilePattern)] -> [FilePattern] -> m [(b, [FilePath])]
filesMatchingWithTag parent' pats ignore = do
  fs <- filesMatching parent' (snd <$> pats) ignore
  let m = Map.fromListWith (<>) $
        flip mapMaybe fs $ \fp -> do
          tag <- getTag pats fp
          pure (tag, one fp)
  pure $ Map.toList m

getTag :: [(b, FilePattern)] -> FilePath -> Maybe b
getTag pats fp =
  let pull patterns =
        listToMaybe $
          flip mapMaybe patterns $ \(tag, pattern) -> do
            guard $ pattern ?== fp
            pure tag
   in if isRelative fp
        then pull pats
        else -- `fp` is an absolute path (because of use of symlinks), so let's
        -- be more lenient in matching it. Note that this does meat we might
        -- match files the user may not have originally intended. This is
        -- the trade offs with using symlinks.
          pull $ second ("**/" <>) <$> pats

data FileAction = Update | Delete
  deriving (Eq, Show)

onChange ::
  forall m.
  (MonadIO m, MonadLogger m, MonadUnliftIO m) =>
  FilePath ->
  -- | The filepath is relative to the folder being monitored, unless if its
  -- ancestor is a symlink.
  (FilePath -> FileAction -> m ()) ->
  m ()
onChange parent' f = do
  -- NOTE: It is important to use canonical path, because this will allow us to
  -- transform fsnotify event's (absolute) path into one that is relative to
  -- @parent'@ (as passed by user), which is what @f@ will expect.
  parent <- liftIO $ canonicalizePath parent'
  withManagerM $ \mgr -> do
    log LevelInfo $ toText $ "Monitoring " <> parent <> " for changes"
    stop <- watchTreeM mgr parent (const True) $ \event -> do
      log LevelDebug $ show event
      let rel = makeRelative parent
      case event of
        Added (rel -> fp) _ _ -> f fp Update
        Modified (rel -> fp) _ _ -> f fp Update
        Removed (rel -> fp) _ _ -> f fp Delete
        Unknown (rel -> fp) _ _ -> f fp Delete
    liftIO $ threadDelay maxBound `finally` stop

withManagerM ::
  (MonadIO m, MonadUnliftIO m) =>
  (WatchManager -> m a) ->
  m a
withManagerM f = do
  withRunInIO $ \run ->
    withManager $ \mgr -> run (f mgr)

watchTreeM ::
  forall m.
  (MonadIO m, MonadUnliftIO m) =>
  WatchManager ->
  FilePath ->
  ActionPredicate ->
  (Event -> m ()) ->
  m StopListening
watchTreeM wm fp pr f =
  withRunInIO $ \run ->
    watchTree wm fp pr $ \evt -> run (f evt)

log :: MonadLogger m => LogLevel -> Text -> m ()
log = logWithoutLoc "Emanote.Source.Mount"
