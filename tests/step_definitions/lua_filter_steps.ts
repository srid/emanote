/**
 * Lua-filter-specific steps. These shape the file content the
 * scenarios write — a Pandoc Lua filter that rewrites a known token,
 * or a Markdown note that references a (possibly-missing) Lua filter.
 *
 * Generic file mutation primitives (`I delete`, page-content
 * assertions) live in `notebook_edit_steps.ts` and are reused by any
 * future dep-kind feature (YAML cascade, transclude, etc.).
 */

import { When } from "@cucumber/cucumber";
import * as fs from "node:fs";
import * as path from "node:path";
import { stagedFixtureDir } from "../support/fixture.ts";

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
    const target = path.join(stagedFixtureDir, fp);
    fs.mkdirSync(path.dirname(target), { recursive: true });
    fs.writeFileSync(target, tokenRewriteFilter(fromToken, toToken));
  },
);

// Used to set up the missing-at-parse-time case: the .md is created
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
    fs.writeFileSync(path.join(stagedFixtureDir, mdPath), md);
  },
);

When(
  "I write an Org note {string} that references missing filter {string} containing token {string}",
  function (orgPath: string, filterPath: string, token: string) {
    const org = `#+TITLE: Late-Bound Org Filter Test
#+PANDOC_FILTERS: ${filterPath}

* Late-Bound Org Filter Test

A token a not-yet-existing filter will rewrite: ${token}
`;
    fs.writeFileSync(path.join(stagedFixtureDir, orgPath), org);
  },
);
