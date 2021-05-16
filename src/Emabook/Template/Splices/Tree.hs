{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Emabook.Template.Splices.Tree (treeSplice, TreeLoc (..)) where

import qualified Data.List.NonEmpty as NE
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
  let getChildTagClass tag extra = do
        child <- XmlHtml.childElementTag tag node
        nonEmpty $
          catMaybes $
            [ XmlHtml.getAttribute "class" child
            ]
              <> ( extra
                     <&> \prefix ->
                       XmlHtml.getAttribute (prefix <> ":class") child
                 )
  pure $ RX.renderHtmlNodes $ go getChildTagClass [] tree
  where
    go getChildCls parSlugs (xs :: [Tree a]) = do
      let childClsAttr tag states =
            maybe mempty (A.class_ . H.toValue . T.intercalate " " . toList) $
              getChildCls tag states
          subTreeCls =
            childClsAttr "forest" $
              [ "level:" <> show (length parSlugs)
              ]
                <> case getTreeLoc <$> nonEmpty (reverse parSlugs) of
                  Just TreeLoc_Current -> ["active", "active:current"]
                  Just TreeLoc_Ancestor -> ["active", "active:ancestor"]
                  Just TreeLoc_Child -> ["active", "active:child"]
                  Just TreeLoc_Elsewhere -> ["inactive"]
                  Nothing -> []
      H.div ! subTreeCls $ do
        forM_ xs $ \(Node slug children) -> do
          let nodeState =
                if null parSlugs || not (null children)
                  then TreeItem_RootOrParent
                  else TreeItem_Terminal
              nodeCls = childClsAttr "node" (one $ show nodeState)
          H.div ! nodeCls $ do
            let itemPath = NE.reverse $ slug :| parSlugs
                linkState =
                  case getTreeLoc itemPath of
                    TreeLoc_Current -> TreeLink_Active
                    _ -> TreeLink_Inactive
                linkCls = childClsAttr "link" (one $ show linkState)
                nodeRoute = pathToRoute $ NE.reverse $ slug :| parSlugs
            H.a ! linkCls ! A.href (H.toValue $ Ema.routeUrl nodeRoute) $
              itemRender nodeRoute
          go getChildCls ([slug] <> parSlugs) children

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

data TreeItemState
  = TreeItem_RootOrParent
  | TreeItem_Terminal
  deriving (Eq, Enum, Bounded)

instance Show TreeItemState where
  show = \case
    TreeItem_RootOrParent -> "parent"
    TreeItem_Terminal -> "terminal"

data TreeLinkState
  = TreeLink_Active
  | TreeLink_Inactive
  deriving (Eq, Enum, Bounded)

instance Show TreeLinkState where
  show = \case
    TreeLink_Active -> "active"
    TreeLink_Inactive -> "inactive"
