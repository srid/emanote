{-# LANGUAGE RecordWildCards #-}

module Emanote.Pandoc.Renderer
  ( NoteRenderers (..),
    PandocInlineRenderer,
    PandocBlockRenderer,
    mkRenderCtxWithNoteRenderers,
  )
where

import qualified Ema.CLI
import Emanote.Model.Type (Model)
import Emanote.Route (LMLRoute)
import Heist (HeistT)
import qualified Heist.Extra.Splices.Pandoc as Splices
import qualified Heist.Extra.Splices.Pandoc.Ctx as Splices
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

-- | Custom Heist renderer for specific Pandoc AST nodes
type PandocRenderer astType n i b x =
  Ema.CLI.Action ->
  Model ->
  NoteRenderers n i b ->
  Splices.RenderCtx n ->
  x ->
  astType ->
  Maybe (HI.Splice n)

type PandocInlineRenderer n i b x = PandocRenderer B.Inline n i b x

type PandocBlockRenderer n i b x = PandocRenderer B.Block n i b x

-- | Custom render logic for a note (available at a LMLRoute)
--
-- Note that unlike Pandoc filters which operate on entire document, these are
-- modeled based on Text.Pandoc.Walk, ie. fine-grained on individual inline and
-- block processing. We do this only so as to render a specific node during
-- recursion (cf. `rpBlock` and `rpInline` in Render.hs).
--
-- So we expect the extensions to be in Haskell, however external script may be
-- supported using a traditional whole-AST extension API.
data NoteRenderers n i b = NoteRenderers
  { noteInlineRenderers :: [PandocInlineRenderer n i b i],
    noteBlockRenderers :: [PandocBlockRenderer n i b b]
  }

mkRenderCtxWithNoteRenderers ::
  forall i b m n.
  (Monad m, Monad n) =>
  NoteRenderers n i b ->
  Map Text Text ->
  Ema.CLI.Action ->
  Model ->
  i ->
  b ->
  HeistT n m (Splices.RenderCtx n)
mkRenderCtxWithNoteRenderers nr@NoteRenderers {..} classRules emaAction model i b =
  Splices.mkRenderCtx
    classRules
    ( \ctx blk ->
        asum $
          noteBlockRenderers <&> \f ->
            f emaAction model nr ctx b blk
    )
    ( \ctx blk ->
        asum $
          noteInlineRenderers <&> \f ->
            f emaAction model nr ctx i blk
    )
