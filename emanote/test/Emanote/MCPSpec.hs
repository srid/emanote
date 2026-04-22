module Emanote.MCPSpec where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Emanote.MCP
import MCP.Protocol (CallToolParams (..), CallToolResult (..))
import Relude (isJust)
import Test.Hspec

spec :: Spec
spec = do
  describe "readPhase1Resource" $ do
    it "serves the phase-1 info resource" $ do
      readPhase1Resource "emanote://phase-1/info"
        `shouldSatisfy` either (const False) (T.isInfixOf "phase 1 is enabled")

    it "rejects unknown resources" $ do
      readPhase1Resource "emanote://missing"
        `shouldBe` Left "Unknown MCP resource: emanote://missing"

  describe "callPhase1Tool" $ do
    it "returns structured status for the phase-1 tool" $ do
      let result = callPhase1Tool (CallToolParams "emanote_phase_1_status" Nothing)
      Aeson.decode @Aeson.Value (Aeson.encode $ structuredContent result)
        `shouldSatisfy` isJust

    it "marks unknown tools as errors" $ do
      isError (callPhase1Tool (CallToolParams "unknown" Nothing))
        `shouldBe` Just True
