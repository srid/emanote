{-# LANGUAGE OverloadedStrings #-}

-- | Shared server identity facts exposed by the Emanote MCP server.
module Emanote.MCP.Info (
  implementationInfo,
  protocolVersion,
) where

import Data.Version (showVersion)
import MCP.Types qualified as MT
import Paths_emanote qualified
import Relude

protocolVersion :: Text
protocolVersion = MT.mcpProtocolVersion

-- | Canonical server identity advertised during MCP initialization.
implementationInfo :: MT.Implementation
implementationInfo =
  MT.Implementation "emanote-mcp" (Just "Emanote MCP") (toText $ showVersion Paths_emanote.version)
