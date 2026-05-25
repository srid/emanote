--- wordcount.lua - append a word/character count footer to the document.
---
--- Derived from https://github.com/pandoc/lua-filters/tree/master/wordcount,
--- but adapted for Emanote's live-server pipeline: the upstream filter
--- prints to stdout and calls `os.exit(0)`, both of which would corrupt or
--- terminate `emanote run`. This version walks the body, counts, and
--- appends a small styled footer block to the rendered document instead.
---
--- FORMAT-agnostic: produces the same output regardless of the writer.

local function new_counts()
  return {
    words = 0,
    characters = 0,
    characters_and_spaces = 0,
  }
end

local function count_filter(counts)
  return {
    Str = function(el)
      if el.text:match("%P") then
        counts.words = counts.words + 1
      end
      counts.characters = counts.characters + utf8.len(el.text)
      counts.characters_and_spaces = counts.characters_and_spaces + utf8.len(el.text)
    end,

    Space = function()
      counts.characters_and_spaces = counts.characters_and_spaces + 1
    end,

    Code = function(el)
      local _, n = el.text:gsub("%S+", "")
      counts.words = counts.words + n
      local nospace = el.text:gsub("%s", "")
      counts.characters = counts.characters + utf8.len(nospace)
      counts.characters_and_spaces = counts.characters_and_spaces + utf8.len(el.text)
    end,

    CodeBlock = function(el)
      local _, n = el.text:gsub("%S+", "")
      counts.words = counts.words + n
      local nospace = el.text:gsub("%s", "")
      counts.characters = counts.characters + utf8.len(nospace)
      counts.characters_and_spaces = counts.characters_and_spaces + utf8.len(el.text)
    end,
  }
end

local css = [[
<style>
.wordcount-footer {
  margin-top: 3rem;
  padding-top: 0.75rem;
  border-top: 1px solid var(--color-gray-200);
  display: flex;
  justify-content: flex-end;
  font-size: 0.8125rem;
  color: var(--color-gray-500);
}
.wordcount-footer dl {
  display: flex;
  gap: 1.5rem;
  margin: 0;
}
.wordcount-footer div { display: flex; gap: 0.375rem; align-items: baseline; }
.wordcount-footer dt { font-variant: small-caps; letter-spacing: 0.04em; }
.wordcount-footer dd {
  margin: 0;
  font-variant-numeric: tabular-nums;
  font-weight: 500;
  color: var(--color-gray-800);
}
.dark .wordcount-footer { border-top-color: var(--color-gray-800); color: var(--color-gray-400); }
.dark .wordcount-footer dd { color: var(--color-gray-100); }
</style>
]]

local function dlEntry(label, value)
  return string.format(
    "<div><dt>%s</dt><dd>%d</dd></div>",
    label, value
  )
end

function Pandoc(doc)
  local counts = new_counts()
  pandoc.walk_block(pandoc.Div(doc.blocks), count_filter(counts))
  local html = table.concat({
    "<dl>",
    dlEntry("words", counts.words),
    dlEntry("chars", counts.characters),
    dlEntry("chars + spaces", counts.characters_and_spaces),
    "</dl>",
  })
  local footer = pandoc.Div(
    { pandoc.RawBlock("html", html) },
    pandoc.Attr("", { "wordcount-footer" }, {})
  )
  table.insert(doc.blocks, pandoc.RawBlock("html", css))
  table.insert(doc.blocks, footer)
  return doc
end
