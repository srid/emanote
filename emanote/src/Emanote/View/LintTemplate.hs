{- | Surface warnings when a Heist template references a splice that has no
binding. Closes <https://github.com/srid/emanote/issues/81>.

Emanote uses interpreted Heist, where 'Heist.Interpreted.runNode' leaves an
element verbatim when its name has no binding, and 'getAttributeSplice'
falls back to the literal @${name}@ text when @name@ has no binding. Both
paths fail silently in the rendered HTML. We re-parse the rendered output
and warn on either signal.
-}
module Emanote.View.LintTemplate (
  warnUnboundSplices,

  -- * Internals (exposed for testing)
  UnboundSplice (..),
  scanRenderedHtml,
  formatWarning,
) where

import Control.Monad.Logger (MonadLogger)
import Data.Set qualified as Set
import Data.Text qualified as T
import Ema qualified
import Emanote.Prelude (logW)
import GHC.IO.Unsafe (unsafePerformIO)
import Relude
import Text.XmlHtml qualified as X

-- | An unbound splice reference detected in rendered HTML output.
data UnboundSplice
  = SpliceElement Text
  | SpliceAttribute Text
  deriving stock (Eq, Ord, Show)

-- | Render a warning as user-facing text.
formatWarning :: UnboundSplice -> Text
formatWarning = \case
  SpliceElement nm -> "<" <> nm <> "/>"
  SpliceAttribute nm -> "${" <> nm <> "}"

{- | Inspect a rendered HTML asset for unbound splice references; log one
warning per fresh @(route, splice)@ pair via 'logW'.
-}
warnUnboundSplices ::
  (MonadIO m, MonadLogger m) =>
  -- | URL of the route whose asset we are linting (used as a label in logs
  -- and as the dedup key).
  Text ->
  Ema.Asset LByteString ->
  m ()
warnUnboundSplices routeUrl = \case
  Ema.AssetGenerated Ema.Html bytes -> do
    let warnings = scanRenderedHtml (toString routeUrl) (toStrict bytes)
    fresh <- recordOnce routeUrl warnings
    forM_ fresh $ \w ->
      logW $ "Unbound template splice on '" <> routeUrl <> "': " <> formatWarning w
  _ -> pass

{- | Re-parse rendered HTML and collect every unbound splice reference. Returns
a deduplicated, sorted list. An unparseable document yields no warnings —
broken HTML is a separate concern.
-}
scanRenderedHtml :: FilePath -> ByteString -> [UnboundSplice]
scanRenderedHtml fp bs = case X.parseHTML fp bs of
  Right (X.HtmlDocument _ _ nodes) -> Set.toAscList (foldMap nodeSplices nodes)
  _ -> []

nodeSplices :: X.Node -> Set UnboundSplice
nodeSplices = \case
  X.Element name attrs children ->
    elementSplice name
      <> foldMap attrSplices attrs
      <> foldMap nodeSplices children
  _ -> mempty

elementSplice :: Text -> Set UnboundSplice
elementSplice name
  | T.any (== ':') name = one (SpliceElement name)
  | otherwise = mempty

attrSplices :: (Text, Text) -> Set UnboundSplice
attrSplices (_, value) = Set.fromList (SpliceAttribute <$> attrSpliceRefs value)

-- | Extract the names from any @${name}@ tokens in a string.
attrSpliceRefs :: Text -> [Text]
attrSpliceRefs t = case T.breakOn "${" t of
  (_, "") -> []
  (_, rest) ->
    let body = T.drop 2 rest
     in case T.breakOn "}" body of
          (name, suffix)
            | "}" `T.isPrefixOf` suffix -> name : attrSpliceRefs (T.drop 1 suffix)
            | otherwise -> []

{- | Process-wide dedup so live-server re-renders do not log the same warning
on every refresh.
-}
{-# NOINLINE alreadyLogged #-}
alreadyLogged :: IORef (Set (Text, UnboundSplice))
alreadyLogged = unsafePerformIO (newIORef mempty)

recordOnce :: (MonadIO m) => Text -> [UnboundSplice] -> m [UnboundSplice]
recordOnce routeUrl ws = liftIO $ atomicModifyIORef' alreadyLogged $ \seen ->
  let entries = Set.fromList ((routeUrl,) <$> ws)
      fresh = Set.difference entries seen
   in (Set.union seen entries, snd <$> Set.toAscList fresh)
