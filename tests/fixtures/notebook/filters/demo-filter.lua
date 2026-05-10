-- Pandoc Lua filter consumed by the e2e Lua-filter hot-reload tests
-- (issue #263). The filter rewrites a single sentinel token; @live-edit
-- scenarios overwrite this file with a different output token and
-- assert the dependent note's rendered HTML reflects the change
-- without restarting `emanote run`.

function Str(el)
  if el.text == "EMANOTE_LUA_DEMO_TOKEN" then
    return pandoc.Str("DEMO_FILTER:HELLO")
  end
  if el.text == "EMANOTELUAORGDEMO" then
    return pandoc.Str("DEMO_FILTER:HELLO")
  end
  return nil
end
