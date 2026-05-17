/**
 * Lua-filter-specific steps. These shape the file content the
 * scenarios write — a Pandoc Lua filter that rewrites a known token,
 * or a note that references a (possibly-missing) Lua filter.
 *
 * Generic file mutation primitives (`I delete`, page-content
 * assertions) live in `notebook_edit_steps.ts` and are reused by any
 * future dep-kind feature (YAML cascade, transclude, etc.).
 */

import { When } from "@cucumber/cucumber";
import { writeStaged } from "../support/fixture.ts";

/** Build a Pandoc Lua filter that rewrites a single token. The shape
 *  is fixed across our scenarios — only the input/output tokens vary
 *  — so the step doesn't need a multi-line doc-string payload. */
function tokenRewriteFilter(fromToken: string, toToken: string): string {
  return `function Str(el)
  if el.text == "${fromToken}" then
    return pandoc.Str("${toToken}")
  end
  return nil
end
`;
}

When(
  "I write {string} so {word} maps to {string}",
  function (fp: string, fromToken: string, toToken: string) {
    writeStaged(fp, tokenRewriteFilter(fromToken, toToken));
  },
);

// Build a CodeBlock-matching hello.lua-shaped filter with a configurable
// greeting prefix. Used by the @hot-reload scenarios that shadow the
// bundled `lua-filters/hello.lua` to prove that editing the .lua source
// of a CodeBlock-matching filter reloads dependent notes.
function helloShadowFilter(prefix: string): string {
  return `local function on_hello(block)
  if block.classes[1] ~= 'hello' then return nil end
  local items = {}
  for line in block.text:gmatch('[^\\n]+') do
    table.insert(items, { pandoc.Plain {
      pandoc.Str('${prefix} '),
      pandoc.Strong { pandoc.Str(line) },
    } })
  end
  return pandoc.BulletList(items)
end
return { { CodeBlock = on_hello } }
`;
}

When(
  "I write a hello-shadow filter at {string} with prefix {string}",
  function (fp: string, prefix: string) {
    writeStaged(fp, helloShadowFilter(prefix));
  },
);

// Used to set up the missing-at-parse-time case: the note is created
// before its referenced filter exists. `parseNote` registers a dep
// edge keyed by the (still-unresolved) frontmatter path; when the
// .lua is later created, the existing edge fires and the note
// re-renders with the filter applied.
When(
  "I write a note {string} that references missing filter {string} containing token {string}",
  function (mdPath: string, filterPath: string, token: string) {
    const md = `---
pandoc:
  filters:
    parse:
      - ${filterPath}
---

# Late-Bound Filter Test

A token a not-yet-existing filter will rewrite: ${token}
`;
    writeStaged(mdPath, md);
  },
);

When(
  "I write an Org note {string} that references missing filter {string} containing token {string}",
  function (orgPath: string, filterPath: string, token: string) {
    const org = `#+TITLE: Late-Bound Org Filter Test
#+PANDOC_FILTERS_PARSE: ${filterPath}

* Late-Bound Org Filter Test

A token a not-yet-existing filter will rewrite: ${token}
`;
    writeStaged(orgPath, org);
  },
);
