/**
 * Owns the fixture-staging volatility: hot-reload scenarios need to
 * mutate files inside the notebook to exercise dependency-driven
 * re-rendering, but the source `tests/fixtures/notebook/` tree must
 * stay clean across runs. We copy the source fixture into a
 * process-scoped tmpdir at import time and point the backend at the
 * copy. Mutating steps write directly into the staged copy; a Before
 * hook tagged @hot-reload resets it between scenarios.
 *
 * The infrastructure is dep-kind-agnostic by design — today only
 * Pandoc Lua filters use it (issue #263), but the same staging,
 * reset, and tag plumbing applies to any future "X depends on Y"
 * relation (e.g. cascaded `index.yaml`).
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

export const sourceFixtureDir = path.resolve(
  import.meta.dirname,
  "..",
  "fixtures",
  "notebook",
);

const stagedRoot = fs.mkdtempSync(path.join(os.tmpdir(), "emanote-e2e-stage-"));

export const stagedFixtureDir = path.join(stagedRoot, "notebook");
fs.cpSync(sourceFixtureDir, stagedFixtureDir, { recursive: true });

/** Notebook paths (subtrees or top-level files) the hot-reload
 *  scenarios mutate. The reset removes each staged entry and re-copies
 *  from source, which both restores edited files and cleans up any
 *  files a scenario created inside a listed subtree (e.g. a
 *  previously-missing filter). Top-level note files belong here when a
 *  scenario edits the note itself rather than its dependency. New
 *  dep-kinds add their own entry here as they come online. */
const mutatedPaths: string[] = [
  "focus-mode", // UI focus-mode hot-reload fixture
  "filters", // Pandoc Lua filters (issue #263)
  "lua-filter-demo.md", // Markdown note edits via its declared filter
  "lua-filter-org-demo.org", // Org note edits via its declared filter
  "lua-filter-hello-fence.md", // hello.lua hot-reload matrix (prose + fence-body edits)
  ".emanoteignore", // .emanoteignore hot-reload (issue #739)
];

/** Files that hot-reload scenarios create from scratch at the
 *  notebook root and that must be removed before the next scenario.
 *  They are absent from the source fixture by design — for example,
 *  `lua-filter-late.md` and `lua-filter-late.org` exercise the
 *  missing-at-parse-time case (issue #263), and a stray copy in the
 *  source tree would crash `emanote gen` (which treats a missing Lua
 *  filter declaration as a fatal note error). */
const ephemeralFiles: string[] = [
  "lua-filter-late.md",
  "lua-filter-late.org",
  "lua-filters/hello.lua", // notebook-local shadow used by the hello.lua hot-reload scenario
];

/** Reset the staged notebook so the next scenario starts from
 *  baseline. Called from the @hot-reload Before hook in
 *  `support/hooks.ts`.
 *
 *  We rm + cp each declared mutated path rather than walking the
 *  whole notebook so unrelated state (the static-gen output dir, the
 *  user's other notebooks) stays untouched, and so a test-created
 *  file inside a tracked subtree is cleaned up automatically without
 *  the scenario needing to declare it. Top-level ephemeral files are
 *  listed explicitly. */
/** Write a file at @relativeFp inside the staged notebook, creating
 *  any missing parent directories. Shared by every step that mutates
 *  the staged fixture — keeps the three-line mkdirSync + writeFileSync
 *  ceremony in one place.
 */
export function writeStaged(relativeFp: string, contents: string): void {
  const target = path.join(stagedFixtureDir, relativeFp);
  fs.mkdirSync(path.dirname(target), { recursive: true });
  fs.writeFileSync(target, contents);
}

export function resetStagedNotebookMutations(): void {
  for (const sub of mutatedPaths) {
    const staged = path.join(stagedFixtureDir, sub);
    const source = path.join(sourceFixtureDir, sub);
    fs.rmSync(staged, { recursive: true, force: true });
    if (fs.existsSync(source)) {
      fs.cpSync(source, staged, { recursive: true });
    }
  }
  for (const fp of ephemeralFiles) {
    const target = path.join(stagedFixtureDir, fp);
    if (fs.existsSync(target)) {
      fs.unlinkSync(target);
    }
  }
}
