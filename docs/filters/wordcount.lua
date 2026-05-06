--- wordcount.lua — append a word/character count footer to the document.
---
--- Inspired by https://github.com/pandoc/lua-filters/tree/master/wordcount,
--- but adapted for Emanote's live-server pipeline: the upstream filter
--- prints to stdout and calls `os.exit(0)`, both of which would corrupt or
--- terminate `emanote run`. This version walks the body, counts, and
--- appends a styled footer block to the rendered document instead.
---
--- FORMAT-agnostic: produces the same output regardless of the writer.

local words = 0
local characters = 0
local characters_and_spaces = 0

local count = {
  Str = function(el)
    if el.text:match("%P") then
      words = words + 1
    end
    characters = characters + utf8.len(el.text)
    characters_and_spaces = characters_and_spaces + utf8.len(el.text)
  end,

  Space = function()
    characters_and_spaces = characters_and_spaces + 1
  end,

  Code = function(el)
    local _, n = el.text:gsub("%S+", "")
    words = words + n
    local nospace = el.text:gsub("%s", "")
    characters = characters + utf8.len(nospace)
    characters_and_spaces = characters_and_spaces + utf8.len(el.text)
  end,

  CodeBlock = function(el)
    local _, n = el.text:gsub("%S+", "")
    words = words + n
    local nospace = el.text:gsub("%s", "")
    characters = characters + utf8.len(nospace)
    characters_and_spaces = characters_and_spaces + utf8.len(el.text)
  end,
}

function Pandoc(doc)
  pandoc.walk_block(pandoc.Div(doc.blocks), count)
  local summary = string.format(
    "%d words · %d characters · %d characters (with spaces)",
    words, characters, characters_and_spaces
  )
  local footer = pandoc.Div(
    { pandoc.Para({ pandoc.Emph(pandoc.Str(summary)) }) },
    pandoc.Attr("", { "wordcount-footer" }, {})
  )
  table.insert(doc.blocks, footer)
  return doc
end
