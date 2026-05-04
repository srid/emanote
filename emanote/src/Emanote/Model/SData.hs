{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.SData where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixGen, ixList)
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Emanote.Route qualified as R
import Optics.TH (makeLenses)
import Relude

{- | `S` for "structured". Refers to a per-route data file represented by Aeson
 value. Example: /foo/bar.yaml file.

 `_sdataValue` carries the parse outcome: `Right` on success, `Left` with
 the parse-error message on failure. Using `Either` (rather than two
 fields like `value + Maybe error`) makes invalid states unrepresentable
 — you can't have both a value and an error.
-}
data SData = SData
  { _sdataValue :: Either Text Aeson.Value
  , _sdataRoute :: R.R 'R.Yaml
  -- ^ Location of this data file
  }
  deriving stock (Eq, Ord, Data, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

type SDataIxs = '[R.R 'R.Yaml]

type IxSData = IxSet SDataIxs SData

instance Indexable SDataIxs SData where
  indices =
    ixList
      (ixGen $ Proxy @(R.R 'R.Yaml))

makeLenses ''SData

parseSDataCascading :: R.R 'R.Yaml -> NonEmpty (FilePath, ByteString) -> SData
parseSDataCascading r bs =
  SData (mergeAesons <$> traverse parseOne bs) r
  where
    parseOne (fp, b) =
      first (\e -> toText $ "Failed to parse " <> fp <> " :" <> Yaml.prettyPrintParseException e)
        $ Yaml.decodeEither' b

-- | Later values override former.
mergeAesons :: NonEmpty Aeson.Value -> Aeson.Value
mergeAesons =
  last . NE.scanl1 mergeAeson

{- | Deep-merge two YAML-shaped Aeson values.

Contract — applies uniformly regardless of caller:

  * Objects merge by key, recursively.
  * Arrays concatenate, then deduplicate (left order preserved).
  * Scalars right-win.

The array clause is the load-bearing one and the deliberate divergence
from @aeson-extra@'s @lodashMerge@: lodash aligns arrays by index,
which for list-valued fields like @tags@ silently clobbers cascade
contributions the moment a child note declares any of its own — see
issue #697.

The function is general-purpose. The metadata cascade
('parseSDataCascading', 'Emanote.Model.Meta.getEffectiveRouteMetaWith')
is the case where every clause matters, but non-cascade callers
('Emanote.Model.Note.withAesonDefault',
'Emanote.Model.Note.overrideAesonText',
'Emanote.View.Template.setErrorPageMeta') merge values that contain no
arrays and so rely only on the object/scalar clauses; they share this
single merger by design rather than coincidence.

Stability note: the contract is fixed across keys. If a future change
needs per-key strategies (e.g. \"replace, don't union, for field X\"),
the right boundary is a new module that owns cascade folding and
parameterises strategy — not a flag added here. Callers should keep
treating this function as the universal deep-merge primitive.
-}
mergeAeson :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeAeson (Aeson.Object a) (Aeson.Object b) =
  Aeson.Object $ KM.unionWith mergeAeson a b
mergeAeson (Aeson.Array a) (Aeson.Array b) =
  Aeson.Array $ V.fromList $ ordNub $ V.toList a <> V.toList b
mergeAeson _ b = b

-- TODO: Use https://hackage.haskell.org/package/lens-aeson
lookupAeson :: forall a. (Aeson.FromJSON a) => a -> NonEmpty Text -> Aeson.Value -> a
lookupAeson x (k :| ks) meta =
  fromMaybe x $ do
    Aeson.Object obj <- pure meta
    val <- KM.lookup (fromString . toString $ k) obj
    case nonEmpty ks of
      Nothing -> resultToMaybe $ Aeson.fromJSON val
      Just ks' -> pure $ lookupAeson x ks' val
  where
    resultToMaybe :: Aeson.Result b -> Maybe b
    resultToMaybe = \case
      Aeson.Error _ -> Nothing
      Aeson.Success b -> pure b

-- | Modify a key inside the aeson Value
modifyAeson :: NonEmpty KM.Key -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> Aeson.Value -> Aeson.Value
modifyAeson (k :| ks) f meta =
  case nonEmpty ks of
    Nothing ->
      withObject meta $ \obj ->
        runIdentity $ KM.alterF (pure . f) k obj
    Just ks' ->
      withObject meta $ \obj ->
        runIdentity $ KM.alterF @Identity (\mv -> Identity $ modifyAeson ks' f <$> mv) k obj
  where
    withObject :: Aeson.Value -> (Aeson.Object -> Aeson.Object) -> Aeson.Value
    withObject v g = case v of
      Aeson.Object x -> Aeson.Object $ g x
      x -> x

oneAesonText :: [Text] -> Text -> Aeson.Value
oneAesonText k v =
  case nonEmpty k of
    Nothing ->
      Aeson.String v
    Just (x :| xs) ->
      Aeson.object [(fromString . toString) x Aeson..= oneAesonText (toList xs) v]
