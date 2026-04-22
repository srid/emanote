module Emanote.MCPSpec where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Emanote.MCP.Info
import Emanote.MCP.Surface
import MCP.Protocol (CallToolParams (..), CallToolResult (..))
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "readResource" $ do
    it "serves server information" $ do
      readResource serverInfoResourceUri
        `shouldSatisfy` either (const False) (T.isInfixOf implementationVersion)

    it "rejects unknown resources" $ do
      readResource "emanote://missing"
        `shouldBe` Left "Unknown MCP resource: emanote://missing"

  describe "callTool" $ do
    it "returns structured server information" $ do
      let result = callTool (CallToolParams serverInfoToolName Nothing)
      Aeson.decode @Aeson.Value (Aeson.encode $ structuredContent result)
        `shouldSatisfy` isJust

    it "marks unknown tools as errors" $ do
      isError (callTool (CallToolParams "unknown" Nothing))
        `shouldBe` Just True
