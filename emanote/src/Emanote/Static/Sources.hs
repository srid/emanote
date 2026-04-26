{- | Canonical paths under @_emanote-static@ that contain
class-name-bearing source — read by tools that need to scan them
(currently the Tailwind CLI; future scanners can reuse).

Knowledge about \"where do utility classes live in static files\"
belongs here, not inside any one tool's invocation logic. Centralizing
it means a future linter / asset-pipeline step can reuse the registry
without dragging in unrelated dependencies (e.g. depending on the
Tailwind module for a path it has nothing else to do with).
-}
module Emanote.Static.Sources (
  jsScanPath,
) where

import Paths_emanote qualified
import Relude
import System.FilePath ((</>))

{- | Path under 'Paths_emanote.getDataDir' where the JS module bundle
lives. Behaviors apply Tailwind utility classes via JS at runtime
(@element.className = '...'@), so anything that needs to know about
those classes — Tailwind compile, lint pass, etc. — must scan this dir.
-}
jsScanPath :: IO FilePath
jsScanPath = (</> "_emanote-static" </> "js") <$> Paths_emanote.getDataDir
