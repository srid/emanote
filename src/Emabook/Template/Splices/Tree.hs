{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Template.Splices.Tree (treeSplice) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Tree (Tree (Node))
import Ema (Ema (..))
import qualified Ema
import qualified Heist as H
import qualified Heist.Interpreted as HI
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Renderer.XmlHtml as RX
import qualified Text.Show as Show
import qualified Text.XmlHtml as XmlHtml

-- | Heist splice to render a `Data.Tree` whilst allowing customization of
-- individual styling in templates.
treeSplice ::
  forall a r n model.
  (Monad n, Eq r, Ema model r) =>
  [Tree a] ->
  r ->
  (NonEmpty a -> r) ->
  (r -> H.Html) ->
  HI.Splice n
treeSplice tree here pathToRoute itemRender = do
  node <- H.getParamNode
  let getChildClass name =
        XmlHtml.getAttribute "class" <=< XmlHtml.childElementTag name $ node
      classes =
        Map.mapMaybe id $
          Map.fromList $
            (id &&& getChildClass)
              <$> mconcat
                [ fmap show [minBound @TreeSub .. maxBound],
                  fmap show [minBound @TreeItemState .. maxBound],
                  fmap show [minBound @TreeLinkState .. maxBound]
                ]
  pure $ RX.renderHtmlNodes $ go classes [] tree
  where
    go attrs parSlugs (xs :: [Tree a]) =
      -- TODO: Refactor this traverse a general Data.Tree
      H.div ! classFrom attrs (show TreeSub) $ do
        forM_ xs $ \(Node slug children) -> do
          let itemState =
                if null parSlugs || not (null children)
                  then TreeItem_RootOrParent
                  else TreeItem_Terminal
              itemCls = classFrom attrs (show itemState)
              itemRoute = pathToRoute $ NE.reverse $ slug :| parSlugs
              linkState =
                if here == itemRoute
                  then TreeLink_Active
                  else TreeLink_Inactive
              linkCls = classFrom attrs (show linkState)
          H.div ! itemCls $
            H.a ! linkCls ! A.href (H.toValue $ Ema.routeUrl itemRoute) $
              itemRender itemRoute
          go attrs ([slug] <> parSlugs) children
    classFrom attrs k =
      classFromElse attrs k Nothing
    classFromElse attrs k (mv :: Maybe Text) =
      case Map.lookup k attrs of
        Nothing ->
          maybe mempty (A.class_ . H.toValue) mv
        Just v ->
          A.class_ $ H.toValue v

data TreeSub = TreeSub
  deriving (Eq, Enum, Bounded)

instance Show TreeSub where
  show TreeSub = "sub-tree"

data TreeItemState
  = TreeItem_RootOrParent
  | TreeItem_Terminal
  deriving (Eq, Enum, Bounded)

instance Show TreeItemState where
  show = \case
    TreeItem_RootOrParent -> "item-parent"
    TreeItem_Terminal -> "item-terminal"

data TreeLinkState
  = TreeLink_Active
  | TreeLink_Inactive
  deriving (Eq, Enum, Bounded)

instance Show TreeLinkState where
  show = \case
    TreeLink_Active -> "link-active"
    TreeLink_Inactive -> "link-inactive"
