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
type PandocRenderer astType n x =
  Ema.CLI.Action ->
  Model ->
  NoteRenderers n ->
  Splices.RenderCtx n ->
  x ->
  astType ->
  Maybe (HI.Splice n)

type PandocInlineRenderer n x = PandocRenderer B.Inline n x

type PandocBlockRenderer n x = PandocRenderer B.Block n x

-- | Custom render logic for a note (available at a LMLRoute)
--
-- Note that unlike Pandoc filters which operate on entire document, these are
-- modeled based on Text.Pandoc.Walk, ie. fine-grained on individual inline and
-- block processing. We do this only so as to render a specific node during
-- recursion (cf. `rpBlock` and `rpInline` in Render.hs).
--
-- So we expect the extensions to be in Haskell, however external script may be
-- supported using a traditional whole-AST extension API.
data NoteRenderers n = NoteRenderers
  { noteInlineRenderers :: [PandocInlineRenderer n LMLRoute],
    noteBlockRenderers :: [PandocBlockRenderer n LMLRoute]
  }

mkRenderCtxWithNoteRenderers ::
  (Monad m, Monad n) =>
  NoteRenderers n ->
  Map Text Text ->
  Ema.CLI.Action ->
  Model ->
  LMLRoute ->
  HeistT n m (Splices.RenderCtx n)
mkRenderCtxWithNoteRenderers nf@NoteRenderers {..} classRules emaAction model x =
  Splices.mkRenderCtx
    classRules
    ( \ctx blk ->
        asum $
          noteBlockRenderers <&> \f ->
            f emaAction model nf ctx x blk
    )
    ( \ctx blk ->
        asum $
          noteInlineRenderers <&> \f ->
            f emaAction model nf ctx x blk
    )
