-- Hello-world render-time Lua filter for Emanote.
--
-- Handles `hello` fenced code blocks and demonstrates Emanote's
-- in-place error protocol via the injected `emanote.error_block`
-- helper (see `docs/guide/lua-filters/writing-filters.md`).
--
-- Happy path -> a greeting; sad path -> a protocol-shaped error block
-- that (a) shows inline as a red banner on the live server, and
-- (b) makes `emanote gen` abort the build unless the notebook opts
-- out via `--allow-broken-lua-filters` (the docs site does).

local function on_hello(block)
  if block.classes[1] ~= 'hello' then
    return nil
  end
  local err = block.text:match('^ERROR:%s*(.*)')
  if err then
    return emanote.error_block{
      title = 'hello.lua error',
      message = err,
    }
  end
  -- Each non-empty line is one greeting; the body becomes a bullet list.
  local items = {}
  for line in block.text:gmatch('[^\n]+') do
    table.insert(items, { pandoc.Plain {
      pandoc.Str('👋 Hello, '),
      pandoc.Strong { pandoc.Str(line) },
      pandoc.Str('!'),
    } })
  end
  return pandoc.BulletList(items)
end

return {
  { CodeBlock = on_hello },
}
