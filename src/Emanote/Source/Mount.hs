{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is a fork of Ema.Helper.FileSystem (forked for ghcid-convenience), to
-- be *soon* made a separate Haskell library.
module Emanote.Source.Mount where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, try)
import Control.Monad (foldM)
import Control.Monad.Logger
  ( LogLevel (LevelDebug, LevelError, LevelInfo),
    MonadLogger,
    logWithoutLoc,
  )
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

data UnionPolicy a m
  = UnionPolicyOverlay
  | UnionPolicyMergeWith (a -> a -> m a)

class HasUnionPolicy tag a m where
  getUnionPolicy :: tag -> UnionPolicy a m

-- | Union a non-empty list of elements into a single element, using the union
-- policy defined for the tag associated with the list.
union :: forall tag m a. Monad m => HasUnionPolicy tag a m => tag -> NonEmpty a -> m a
union =
  applyUnionPolicy . getUnionPolicy
  where
    applyUnionPolicy :: Monad m => UnionPolicy a m -> NonEmpty a -> m a
    applyUnionPolicy = \case
      UnionPolicyOverlay ->
        -- First element overlays the next (and rest)
        pure . head
      UnionPolicyMergeWith f ->
        foldM1 f
      where
        foldM1 :: (Monad m) => (a -> a -> m a) -> NonEmpty a -> m a
        foldM1 f (x :| xs) = foldM f x xs

unionMountOnLVar ::
  forall source tag model m.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLogger m,
    Ord source,
    Ord tag
  ) =>
  NonEmpty (source, FilePath) ->
  [(tag, FilePattern)] ->
  [FilePattern] ->
  LVar model ->
  model ->
  (Change source tag -> FileAction () -> m (model -> model)) ->
  m ()
unionMountOnLVar sources pats ignore modelLVar model0 handleAction = do
  LVar.set modelLVar model0
  unionMount sources pats ignore $ \fs act -> do
    doAct <- handleAction fs act
    LVar.modify modelLVar doAct

-- TODO: Abstract in module with StateT / MonadState
newtype OverlayFs source = OverlayFs
  { unOverlayFs :: Map FilePath (Set source)
  }

-- TODO: Replace this with a function taking `NonEmpty source`
emptyOverlayFs :: Ord source => OverlayFs source
emptyOverlayFs = OverlayFs mempty

overlayFsModify :: FilePath -> (Set src -> Set src) -> OverlayFs src -> OverlayFs src
overlayFsModify k f (OverlayFs m) =
  OverlayFs $
    Map.insert k (f $ fromMaybe Set.empty $ Map.lookup k m) m

overlayFsAdd :: Ord src => FilePath -> src -> OverlayFs src -> OverlayFs src
overlayFsAdd fp src =
  overlayFsModify fp $ Set.insert src

overlayFsRemove :: Ord src => FilePath -> src -> OverlayFs src -> OverlayFs src
overlayFsRemove fp src =
  overlayFsModify fp $ Set.delete src

overlayFsLookup :: FilePath -> OverlayFs source -> Maybe (NonEmpty (source, FilePath))
overlayFsLookup fp (OverlayFs m) = do
  sources <- nonEmpty . toList =<< Map.lookup fp m
  pure $ sources <&> \src -> (src, fp)

overlayFsHasFile :: Ord src => src -> FilePath -> OverlayFs src -> Bool
overlayFsHasFile src fp =
  -- maybe False (Set.member fp) . Map.lookup src . unOverlayFs
  undefined

-- Files matched by each tag pattern, each represented by their corresponding
-- file (absolute path) in the individual sources. It is up to the user to union
-- them (for now).
--
-- If a path is represented by Nothing, it means it just got removed from the
-- last source, and the app must remove it from its internal state.
type Change source tag = Map tag (Map FilePath (FileAction (NonEmpty (source, FilePath))))

changeInsert ::
  (Ord source, Ord tag, MonadState (OverlayFs source) m) =>
  source ->
  tag ->
  FilePath ->
  FileAction () ->
  Change source tag ->
  m (Change source tag)
changeInsert src tag fp act ch = do
  fmap snd . flip runStateT ch $ do
    -- First, register this change in the overlayFs
    lift $
      modify $
        (if act == Delete then overlayFsRemove else overlayFsAdd)
          fp
          src
    overlays <- fmap (maybe Delete Update) $ lift $ gets $ overlayFsLookup fp
    gets (Map.lookup tag) >>= \case
      Nothing ->
        modify $ Map.insert tag $ Map.singleton fp overlays
      Just files ->
        modify $ Map.insert tag $ Map.insert fp overlays files

unionMount ::
  forall source tag m.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLogger m,
    Ord source,
    Ord tag
  ) =>
  NonEmpty (source, FilePath) ->
  [(tag, FilePattern)] ->
  [FilePattern] ->
  (Change source tag -> FileAction () -> m ()) ->
  m ()
unionMount sources pats ignore handleAction = do
  void . flip runStateT (emptyOverlayFs @source) $ do
    -- Initial traversal of sources
    changes0 :: Change source tag <-
      fmap snd . flip runStateT Map.empty $ do
        forM_ sources $ \(src, folder) -> do
          taggedFiles <- filesMatchingWithTag folder pats ignore
          forM_ taggedFiles $ \(tag, fs) -> do
            forM_ fs $ \fp -> do
              put =<< lift . changeInsert src tag fp (Update ()) =<< get
    lift $ handleAction changes0 (Update ())
    -- Run fsnotify on sources
    ofs <- get
    -- FIXME: Synchronize so that this is run serially (for state)
    -- FIXME: State result is discarded!!!!!
    lift . onChange (toList sources) $ \src fp act -> void . flip runStateT ofs $ do
      let shouldIgnore = any (?== fp) ignore
      whenJust (guard (not shouldIgnore) >> getTag pats fp) $ \tag -> do
        -- Update our overlay state with this change.
        changes <- fmap snd . flip runStateT Map.empty $ do
          put =<< lift . changeInsert src tag fp (Update ()) =<< get
        lift $ handleAction changes act

-- | Mount the given directory on to the given LVar such that any filesystem
-- events (represented by `FileAction`) are made to be reflected in the LVar
-- model using the given model update function.
mountOnLVar' ::
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
  ([(b, [(s, (Maybe s, FilePath))])] -> FileAction () -> m (model -> model)) ->
  m ()
mountOnLVar' mFallbackFolder (stag, folder) pats ignore var var0 toAction' = do
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
    fbFilesF <- toAction (second (fmap ((stagBase,) . (Nothing,))) <$> fbFiles) $ Update ()
    fs <- filesMatchingWithTag folder pats ignore
    fsF <- toAction (second (fmap withBase) <$> fs) $ Update ()
    pure $ fsF . fbFilesF $ var0
  onChange (((),) <$> [folder]) $ \() fp change -> do
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

mountOnLVar ::
  (MonadUnliftIO m, MonadLogger m, Ord a) =>
  FilePath ->
  [(a, FilePattern)] ->
  [FilePattern] ->
  LVar model ->
  model ->
  ([(a, [FilePath])] -> FileAction () -> m (model -> model)) ->
  m ()
mountOnLVar folder pats ignore modelLVar model0 f = do
  unionMountOnLVar (one ((), folder)) pats ignore modelLVar model0 $ \ch act ->
    f (second Map.keys <$> Map.toList ch) act

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

data FileAction a = Update a | Delete
  deriving (Eq, Show)

onChange ::
  forall x m.
  (MonadIO m, MonadLogger m, MonadUnliftIO m) =>
  [(x, FilePath)] ->
  -- | The filepath is relative to the folder being monitored, unless if its
  -- ancestor is a symlink.
  (x -> FilePath -> FileAction () -> m ()) ->
  m ()
onChange roots f = do
  withManagerM $ \mgr -> do
    stops <- forM roots $ \(x, rootRel) -> do
      -- NOTE: It is important to use canonical path, because this will allow us to
      -- transform fsnotify event's (absolute) path into one that is relative to
      -- @parent'@ (as passed by user), which is what @f@ will expect.
      root <- liftIO $ canonicalizePath rootRel
      log LevelInfo $ toText $ "Monitoring " <> root <> " for changes"
      watchTreeM mgr root (const True) $ \event -> do
        log LevelDebug $ show event
        let rel = makeRelative root
        case event of
          Added (rel -> fp) _ _ -> f x fp $ Update ()
          Modified (rel -> fp) _ _ -> f x fp $ Update ()
          Removed (rel -> fp) _ _ -> f x fp Delete
          Unknown (rel -> fp) _ _ -> f x fp Delete
    liftIO $ threadDelay maxBound `finally` forM_ stops id

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
