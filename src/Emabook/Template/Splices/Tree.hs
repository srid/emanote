{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Template.Splices.Tree (treeSplice, TreeLoc (..)) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
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

data TreeLoc
  = -- | Location is exactly pointing to current active route
    TreeLoc_Current
  | -- | Location is an ancestor of current active route
    TreeLoc_Ancestor
  | -- | Location is direct child of current active route
    TreeLoc_Child
  | -- | Elsewhere in tree.
    TreeLoc_Elsewhere
  deriving (Eq, Show, Ord)

-- | Heist splice to render a `Data.Tree` whilst allowing customization of
-- individual styling in templates.
treeSplice ::
  forall a r n model.
  (Monad n, Eq r, Ema model r) =>
  [Tree a] ->
  -- | Data associated with a path; For passing to splice.
  (NonEmpty a -> r) ->
  -- | Return the route for the given tree path
  (NonEmpty a -> TreeLoc) ->
  -- TODO: See tpl. We want to distinguish between terminal and parent item
  -- So just take a child splice, passing it the contents (that's link title and url)
  -- Thus avoid Ema knowledge.
  (r -> H.Html) ->
  HI.Splice n
treeSplice tree pathToRoute getTreeLoc itemRender = do
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
  pure $ RX.renderHtmlNodes $ go getChildClass classes [] tree
  where
    go getChildCls attrs parSlugs (xs :: [Tree a]) = do
      let childClsAttr tags =
            A.class_ $ H.toValue $ T.intercalate " " $ mapMaybe getChildCls tags
          subTreeCls =
            childClsAttr $
              [ show TreeSub,
                show TreeSub <> ":level:" <> show (length parSlugs)
              ]
                <> case getTreeLoc <$> nonEmpty (reverse parSlugs) of
                  Just TreeLoc_Current -> ["tree:active", "tree:active:current"]
                  Just TreeLoc_Ancestor -> ["tree:active", "tree:active:ancestor"]
                  Just TreeLoc_Child -> ["tree:active", "tree:active:child"]
                  Just TreeLoc_Elsewhere -> ["tree:inactive"]
                  Nothing -> []
      H.div ! subTreeCls $ do
        forM_ xs $ \(Node slug children) -> do
          let itemPath = NE.reverse $ slug :| parSlugs
              itemState =
                if null parSlugs || not (null children)
                  then TreeItem_RootOrParent
                  else TreeItem_Terminal
              itemCls = classFrom attrs (one $ show itemState)
              itemRoute = pathToRoute $ NE.reverse $ slug :| parSlugs
              linkState =
                case getTreeLoc itemPath of
                  TreeLoc_Current -> TreeLink_Active
                  _ -> TreeLink_Inactive
              linkCls = classFrom attrs (one $ show linkState)
          H.div ! itemCls $
            H.a ! linkCls ! A.href (H.toValue $ Ema.routeUrl itemRoute) $
              itemRender itemRoute
          go getChildCls attrs ([slug] <> parSlugs) children
    classFrom attrs ks =
      classFromElse attrs ks Nothing
    classFromElse attrs ks (mv :: Maybe Text) =
      let vs = mapMaybe (`Map.lookup` attrs) ks
          mCls = maybe mv (Just . T.intercalate " " . toList) $ nonEmpty vs
       in maybe mempty (A.class_ . H.toValue) mCls

data TreeSub = TreeSub
  deriving (Eq, Enum, Bounded)

instance Show TreeSub where
  show TreeSub = "tree"

data TreeItemState
  = TreeItem_RootOrParent
  | TreeItem_Terminal
  deriving (Eq, Enum, Bounded)

instance Show TreeItemState where
  show = \case
    TreeItem_RootOrParent -> "item:parent"
    TreeItem_Terminal -> "item:terminal"

data TreeLinkState
  = TreeLink_Active
  | TreeLink_Inactive
  deriving (Eq, Enum, Bounded)

instance Show TreeLinkState where
  show = \case
    TreeLink_Active -> "link:active"
    TreeLink_Inactive -> "link:inactive"
