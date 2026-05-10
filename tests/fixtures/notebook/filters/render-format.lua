-- Pandoc Lua filter consumed by the e2e render-time Lua-filter tests.
-- The filter only runs for HTML rendering; if Emanote applies it during
-- parse-time Markdown filtering, the FORMAT guard keeps the token intact.

if FORMAT ~= "html" then
  return {}
end

function Str(el)
  if el.text == "EMANOTE_RENDER_FILTER_TOKEN" then
    return pandoc.Str("RENDER_FILTER:HTML")
  end
  if el.text == "EMANOTEORGRENDERFILTERTOKEN" then
    return pandoc.Str("RENDER_FILTER:ORG-HTML")
  end
  return nil
end
