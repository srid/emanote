{- | MCP (Model Context Protocol) server for Emanote.

Exposes the notebook model as read-only MCP resources:

* @emanote:\/\/export\/metadata@ — JSON metadata for every note
* @emanote:\/\/export\/content@ — all notes concatenated as a single Markdown document
* @emanote:\/\/note\/{path}@ — an individual note by its source path

Umbrella module. Implementation lives in:

* "Emanote.MCP.Catalog" — notebook resource catalog (what's available, how to read it)
* "Emanote.MCP.Uri" — URI wire schema and 'ResourceKind' \<-\> URI translation
* "Emanote.MCP.Handlers" — request handlers adapting the catalog to MCP wire types
* "Emanote.MCP.Server" — Warp setup, server identity, capabilities, instructions
-}
module Emanote.MCP (
  run,
) where

import Emanote.MCP.Server (run)
