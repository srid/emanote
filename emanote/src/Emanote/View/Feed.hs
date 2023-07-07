{-# LANGUAGE NamedFieldPuns #-}

module Emanote.View.Feed where

import Data.Aeson qualified as Aeson
import Data.Aeson.Optics (key, _String)
import Emanote.Model (Model)
import Emanote.Model.Meta (getEffectiveRouteMeta)
import Emanote.Model.Note (Feed (..), Note (..), lookupMeta)
import Emanote.Model.Query (Query, parseQuery, runQuery)
import Emanote.Model.SData (lookupAeson)
import Emanote.Model.Title (toPlain)
import Emanote.Route.SiteRoute
import Emanote.Route.SiteRoute.Class (noteFeedSiteRoute)
import Optics.Operators ((^?))
import Optics.Optic ((%))
import Relude
import Text.Atom.Feed qualified as Atom
import Text.Atom.Feed.Export qualified as Export (textFeed)
import Text.Pandoc.Definition hiding (lookupMeta)

import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

feedDiscoveryLink :: Model -> Note -> Html
feedDiscoveryLink model note =
  H.link
    ! A.href ("/" <> H.toValue feedUrl)
    ! A.rel "alternate"
    ! A.type_ "application/atom+xml"
    ! A.title "Atom Feed"
  where
    feedUrl = siteRouteUrl model (noteFeedSiteRoute note)

noteToEntry :: Atom.URI -> (Note -> Text) -> Note -> Atom.Entry
noteToEntry baseUrl noteUrl note = entry {Atom.entrySummary, Atom.entryLinks}
  where
    entry = Atom.nullEntry entryUrl noteTitle noteDate
    entryUrl = noteUrl note
    entryLinks = [(Atom.nullLink (baseUrl <> "/" <> entryUrl)) {Atom.linkRel = Just (Left "alternate")}]
    noteDate = getNoteDate note
    noteTitle = Atom.TextString $ toPlain $ _noteTitle note
    entrySummary = Atom.TextString <$> lookupMeta ("page" :| ["description"]) note

getNoteDate :: Note -> Atom.Date
getNoteDate note = fromMaybe "1970-01-01" $ _noteMeta note ^? key "date" % _String

getNoteQuery :: Note -> Either LText Query
getNoteQuery note = case _noteDoc note of
  Pandoc _meta [] -> Left "empty note"
  Pandoc _meta blocks -> go blocks
  where
    go [] = Left "can't find note query"
    go (block : rest) = case block of
      CodeBlock ("", classes, _) txt | "query" `elem` classes -> case parseQuery txt of
        Nothing -> Left ("invalid query: " <> toLazy txt)
        Just query -> case go rest of
          -- Check that only query exists
          Right _ -> Left "multiple ```query found"
          Left _ -> Right query
      _ -> go rest

renderFeed :: Model -> Note -> Either LText LByteString
renderFeed model baseNote = case eFeedText of
  Left err -> Left err
  Right feedText -> Right (encodeUtf8 feedText)
  where
    eFeedText = do
      -- get the note feed
      feed <- maybeToRight "feed attribute missing" $ _noteFeed baseNote

      -- find the query and get the feed notes
      feedQuery <- getNoteQuery baseNote
      notes <- case runQuery (_noteRoute baseNote) model feedQuery of
        [] -> Left "no notes matched the query"
        x : xs -> Right (x :| xs)

      -- lookup the feedUrl
      let feedMeta :: Aeson.Value
          feedMeta = getEffectiveRouteMeta (_noteRoute baseNote) model
      let mFeedUrl :: Maybe Text
          mFeedUrl = lookupAeson Nothing ("feed" :| ["siteUrl"]) feedMeta
      feedUrl <- maybeToRight "index.yaml or note doesn't have feed.siteUrl" mFeedUrl

      -- process the notes
      let noteUrl note =
            let sr = SiteRoute_ResourceRoute $ ResourceRoute_LML LMLView_Html $ _noteRoute note
             in siteRouteUrl model sr
      let takeNotes = case _feedLimit feed of
            Nothing -> id
            Just x -> take (fromIntegral x)
      let feedEntries = noteToEntry feedUrl noteUrl <$> takeNotes (toList notes)

      -- render the feed
      let feedTitle = fromMaybe (toPlain $ _noteTitle baseNote) (_feedTitle feed)
      let feedName = Atom.TextString feedTitle
      let feedUpdated = getNoteDate (head notes)
      let feedLinks =
            [ (Atom.nullLink (feedUrl <> "/" <> noteUrl baseNote)) {Atom.linkRel = Just (Left "alternate")}
            , (Atom.nullLink (feedUrl <> "/" <> siteRouteUrl model (noteFeedSiteRoute baseNote))) {Atom.linkRel = Just (Left "self")}
            ]
      let atomFeed = (Atom.nullFeed feedUrl feedName feedUpdated) {Atom.feedEntries, Atom.feedLinks}
      maybeToRight "invalid feed" $ Export.textFeed atomFeed
