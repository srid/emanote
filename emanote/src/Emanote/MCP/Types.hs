{-# OPTIONS_GHC -Wno-orphans #-}

{- | Package-level type-family instances for the MCP server.

The dpella/mcp library leaves 'MCPHandlerState' and 'MCPHandlerUser'
open so applications can plug in their own session-state and auth-user
types. Emanote uses neither — the HTTP transport bypasses the JWT
pipeline that would consume 'MCPHandlerUser' — so both collapse to '()'.

These live in their own module so any transport (HTTP today; stdio or
otherwise tomorrow) can import them without depending on
"Emanote.MCP.Handlers".
-}
module Emanote.MCP.Types () where

import MCP.Server (MCPHandlerState, MCPHandlerUser)

type instance MCPHandlerState = ()

type instance MCPHandlerUser = ()
