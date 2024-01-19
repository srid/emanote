{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.SData where

import Data.Aeson qualified as Aeson
import Data.Aeson.Extra.Merge qualified as AesonMerge
import Data.Aeson.KeyMap qualified as KM
import Data.Data (Data)
import Data.IxSet.Typed (Indexable (..), IxSet, ixGen, ixList)
import Data.List.NonEmpty qualified as NE
import Data.Yaml qualified as Yaml
import Emanote.Route qualified as R
import Optics.TH (makeLenses)
import Relude

{- | `S` for "structured". Refers to a per-route data file represented by Aeson
 value.  Example: /foo/bar.yaml file
-}
data SData = SData
  { _sdataValue :: Aeson.Value
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

parseSDataCascading :: R.R 'R.Yaml -> NonEmpty (FilePath, ByteString) -> Either Text SData
parseSDataCascading r bs = do
  vals <- forM bs $ \(fp, b) ->
    (first (\err -> toText $ "Failed to parse " <> fp <> " :" <> Yaml.prettyPrintParseException err) . Yaml.decodeEither') b
  let val = mergeAesons vals
  pure $ SData val r

-- | Later values override former.
mergeAesons :: NonEmpty Aeson.Value -> Aeson.Value
mergeAesons =
  last . NE.scanl1 mergeAeson

mergeAeson :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeAeson = AesonMerge.lodashMerge

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

-- Modify a key inside the aeson Value
modifyAeson :: NonEmpty KM.Key -> (Maybe Aeson.Value -> Maybe Aeson.Value) -> Aeson.Value -> Aeson.Value
modifyAeson (k :| ks) f meta =
  case nonEmpty ks of
    Nothing ->
      case meta of
        Aeson.Object obj ->
          Aeson.Object $ runIdentity $ KM.alterF @Identity (pure . f) k obj
        _ ->
          meta
    Just ks' ->
      case meta of
        Aeson.Object obj ->
          Aeson.Object $ runIdentity $ KM.alterF @Identity (\mv -> Identity $ modifyAeson ks' f <$> mv) k obj
        _ ->
          meta

oneAesonText :: [Text] -> Text -> Aeson.Value
oneAesonText k v =
  case nonEmpty k of
    Nothing ->
      Aeson.String v
    Just (x :| xs) ->
      Aeson.object [(fromString . toString) x Aeson..= oneAesonText (toList xs) v]
