{- | MCP (Model Context Protocol) server for Emanote.

Exposes the notebook model as a single read-only resource plus a set of
query tools:

* @emanote:\/\/export\/metadata@ — JSON metadata for every note
  (titles, source paths, parent routes, resolved links)
* @find_notes@, @get_backlinks@, @resolve_wikilink@ — query tools that
  return note source paths and titles

Per-note bodies are intentionally not served: every result already
includes the note's source-relative @path@, and MCP clients read the
underlying file with their own filesystem tools. This keeps the MCP
surface focused on the structured data Emanote can answer for that
clients can't compute themselves.

Umbrella module. Implementation lives in:

* "Emanote.MCP.Types" — package-level type-family instances
* "Emanote.MCP.Catalog" — notebook resource catalog (what's available, how to read it)
* "Emanote.MCP.Handlers" — request handlers adapting the catalog to MCP wire types
* "Emanote.MCP.Server" — Warp setup, server identity, capabilities, instructions
-}
module Emanote.MCP (
  run,
) where

import Emanote.MCP.Server (run)
