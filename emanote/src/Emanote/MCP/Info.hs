{-# LANGUAGE OverloadedStrings #-}

-- | Shared server identity facts exposed by the Emanote MCP server.
module Emanote.MCP.Info (
  implementationInfo,
  implementationName,
  implementationTitle,
  implementationVersion,
  protocolVersion,
) where

import Data.Version (showVersion)
import MCP.Types qualified as MT
import Paths_emanote qualified
import Relude

implementationName :: Text
implementationName = "emanote-mcp"

implementationTitle :: Text
implementationTitle = "Emanote MCP"

implementationVersion :: Text
implementationVersion = toText $ showVersion Paths_emanote.version

protocolVersion :: Text
protocolVersion = MT.mcpProtocolVersion

-- | Canonical server identity advertised during MCP initialization.
implementationInfo :: MT.Implementation
implementationInfo =
  MT.Implementation implementationName (Just implementationTitle) implementationVersion
