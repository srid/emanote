module Main where

import Emanote (defaultEmanoteConfig, run)
import Emanote.CLI qualified as CLI
import Emanote.Source.Dynamic (emanoteCompileTailwind)
import Main.Utf8 (withUtf8)
import Optics.Core ((.~))
import Relude

main :: IO ()
main =
  withUtf8 $ do
    cli <- CLI.parseCli
    run (defaultEmanoteConfig cli & emanoteCompileTailwind .~ True)
