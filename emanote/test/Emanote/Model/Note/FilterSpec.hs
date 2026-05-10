{-# LANGUAGE OverloadedStrings #-}

module Emanote.Model.Note.FilterSpec where

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Writer.Strict (runWriterT)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Emanote.Model.Note qualified as Note
import Emanote.Model.Note.Filter qualified as NoteFilter
import Emanote.Route qualified as R
import Emanote.Source.Loc (Loc (LocUser))
import Relude
import System.Directory (createDirectoryIfMissing)
import Test.Hspec
import Text.Pandoc.Definition (Block (Para), Inline (Str), Meta (Meta), Pandoc (Pandoc))
import Text.Pandoc.Lua (getEngine)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "parse-time filter IO policy" $ do
    it "rejects direct Lua IO references"
      $ withSystemTempDirectory "emanote-parse-filter-io"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "function Pandoc(doc)\n  io.open('x')\n  return doc\nend\n"
        errors
          `shouldSatisfy` any ("references io" `T.isInfixOf`)

    it "rejects Lua stdout/stderr references"
      $ withSystemTempDirectory "emanote-parse-filter-output-io"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "function Pandoc(doc)\n  print('x')\n  return doc\nend\n"
        errors
          `shouldSatisfy` any ("references print" `T.isInfixOf`)

    it "rejects table-key access to Pandoc IO APIs"
      $ withSystemTempDirectory "emanote-parse-filter-pandoc-io"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "function Pandoc(doc)\n  pandoc['pipe']('dot', {}, '')\n  return doc\nend\n"
        errors
          `shouldSatisfy` any ("references pandoc.pipe" `T.isInfixOf`)

    it "rejects nested Pandoc filter runners"
      $ withSystemTempDirectory "emanote-parse-filter-nested-pandoc-io"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "function Pandoc(doc)\n  pandoc.utils.run_lua_filter(doc, 'x.lua')\n  return doc\nend\n"
        errors
          `shouldSatisfy` any ("references pandoc.utils.run_lua_filter" `T.isInfixOf`)

    it "ignores IO API names in comments and strings"
      $ withSystemTempDirectory "emanote-parse-filter-io-text"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "-- io.open\nlocal x = 'pandoc.pipe'\nfunction Pandoc(doc)\n  return doc\nend\n"
        errors `shouldBe` []

    it "ignores IO API names hidden inside level-N long strings"
      $ withSystemTempDirectory "emanote-parse-filter-long-string"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "local s = [==[ pandoc.pipe and io.open ]==]\nfunction Pandoc(doc)\n  return doc\nend\n"
        errors `shouldBe` []

    it "ignores IO API names hidden inside level-N long comments"
      $ withSystemTempDirectory "emanote-parse-filter-long-comment"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "--[==[ pandoc.pipe and io.open ]==]\nfunction Pandoc(doc)\n  return doc\nend\n"
        errors `shouldBe` []

    it "guards dynamic access to parse-time IO APIs"
      $ withSystemTempDirectory "emanote-parse-filter-dynamic-io"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "function Pandoc(doc)\n  pandoc['pi' .. 'pe']('sh', {'-c', 'echo nope'}, '')\n  return doc\nend\n"
        errors
          `shouldSatisfy` any ("Parse-time Lua filters cannot use IO: pandoc.pipe" `T.isInfixOf`)

    it "guards dynamic nested Pandoc filter runners"
      $ withSystemTempDirectory "emanote-parse-filter-dynamic-nested-io"
      $ \dir -> do
        errors <- parseMarkdownWithFilter dir "function Pandoc(doc)\n  pandoc.utils['run_' .. 'lua_filter'](doc, 'x.lua')\n  return doc\nend\n"
        errors
          `shouldSatisfy` any ("Parse-time Lua filters cannot use IO: pandoc.utils.run_lua_filter" `T.isInfixOf`)

    it "does not leak the parse-time IO guard into render-time filters"
      $ withSystemTempDirectory "emanote-parse-filter-render-io"
      $ \dir -> do
        createDirectoryIfMissing True $ dir <> "/filters"
        writeFileText (dir <> "/filters/parse.lua") "function Pandoc(doc)\n  return doc\nend\n"
        writeFileText (dir <> "/filters/render.lua") "function Pandoc(doc)\n  local out = pandoc.pipe('printf', {'RENDER_IO_OK'}, '')\n  doc.blocks = { pandoc.Para({ pandoc.Str(out) }) }\n  return doc\nend\n"
        engine <- getEngine
        ((parsedDoc, _), parseErrors) <-
          runNoLoggingT
            $ runWriterT
            $ NoteFilter.applyParsePandocFilters
              engine
              [dir]
              (NoteFilter.PandocFilterDeclarations ["filters/parse.lua"] mempty)
              (Pandoc (Meta mempty) [Para [Str "Hello"]])
        parseErrors `shouldBe` []
        (renderedDoc, renderErrors) <-
          runNoLoggingT
            $ runWriterT
            $ NoteFilter.applyRenderHtmlPandocFilters
              engine
              [dir]
              (NoteFilter.PandocFilterDeclarations mempty ["filters/render.lua"])
              Aeson.Null
              parsedDoc
        renderErrors `shouldBe` []
        renderedDoc `shouldBe` Pandoc (Meta mempty) [Para [Str "RENDER_IO_OK"]]

parseMarkdownWithFilter :: FilePath -> Text -> IO [Text]
parseMarkdownWithFilter dir filterSource = do
  createDirectoryIfMissing True $ dir <> "/filters"
  writeFileText (dir <> "/filters/io.lua") filterSource
  engine <- getEngine
  let route = fromMaybe (error "bad test route") $ R.mkLMLRouteFromKnownFilePath R.Md "note.md"
      src = (LocUser 1 dir Nothing, "note.md")
      md = "---\npandoc:\n  filters:\n    parse:\n      - filters/io.lua\n---\n\n# Hello\n"
  result <- runNoLoggingT $ Note.parseNote engine [dir] route src md
  pure $ Note._noteErrors $ Note.parsedNote result
