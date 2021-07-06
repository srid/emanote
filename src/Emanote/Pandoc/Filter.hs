{-# LANGUAGE RecordWildCards #-}

module Emanote.Pandoc.Filter where

import qualified Ema.CLI
import qualified Emanote.Model.Note as MN
import Emanote.Model.Type (Model)
import Emanote.Pandoc.Filter.Builtin (prepareNoteDoc)
import Emanote.Route (LMLRoute)
import Heist (HeistT, Template)
import qualified Heist.Extra.Splices.Pandoc as Splices
import qualified Heist.Interpreted as HI
import qualified Text.Pandoc.Definition as B

type PandocFilter astType n x =
  Ema.CLI.Action ->
  Model ->
  NoteFilters n ->
  Splices.RenderCtx n ->
  x ->
  astType ->
  Maybe (HI.Splice n)

type InlineFilter n x = PandocFilter B.Inline n x

type BlockFilter n x = PandocFilter B.Block n x

-- | Filters to apply when rendering a note (available at a LMLRoute)
data NoteFilters n = NoteFilters
  { noteInlineFilters :: [InlineFilter n LMLRoute],
    noteBlockFilters :: [BlockFilter n LMLRoute]
  }

noteFiltersInlineOnly :: NoteFilters n -> NoteFilters n
noteFiltersInlineOnly nf =
  nf {noteBlockFilters = mempty}

mkRenderCtxWithNoteFilters ::
  (Monad m, Monad n) =>
  NoteFilters n ->
  Map Text Text ->
  Ema.CLI.Action ->
  Model ->
  LMLRoute ->
  HeistT n m (Splices.RenderCtx n)
mkRenderCtxWithNoteFilters nf@NoteFilters {..} classRules emaAction model x =
  Splices.mkRenderCtxWithoutFootnotes
    classRules
    ( \ctx blk ->
        asum $
          noteBlockFilters <&> \f ->
            f emaAction model nf ctx x blk
    )
    ( \ctx blk ->
        asum $
          noteInlineFilters <&> \f ->
            f emaAction model nf ctx x blk
    )

pandocSpliceWithFilters ::
  Monad n =>
  NoteFilters n ->
  Map Text Text ->
  Ema.CLI.Action ->
  Model ->
  LMLRoute ->
  B.Pandoc ->
  HI.Splice n
pandocSpliceWithFilters nf classRules emaAction model x doc = do
  ctx <- mkRenderCtxWithNoteFilters nf classRules emaAction model x
  Splices.pandocSplice ctx doc

-- | Like `pandocSpliceWithFilters` but when it is known that we are rendering a `Note`
--
-- The note will be processed ahead using `prepareNoteDoc`.
noteSpliceWithFilters ::
  Monad n =>
  NoteFilters n ->
  Map Text Text ->
  Ema.CLI.Action ->
  Model ->
  MN.Note ->
  HI.Splice n
noteSpliceWithFilters nf rules act model note =
  pandocSpliceWithFilters nf rules act model (MN._noteRoute note) $
    prepareNoteDoc model $
      MN._noteDoc note