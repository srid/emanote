---
order: 50
slug: emanoteignore
---

# `.emanoteignore`

Drop a file named `.emanoteignore` at the **top of any notebook layer** and Emanote will skip the matching files when building the model — both during `emanote gen` and in the live server.

## File format

One [`FilePattern`][filepattern] per line. Blank lines and lines whose first non-whitespace character is `#` are ignored.

```text
# Obsidian template directory
templates/**

# Daily journal — kept private
journal/**

# Local node_modules from a remark/MDX setup
node_modules/**
```

A `#` that appears mid-line is a literal character, not a comment marker.

> **Note**: `.emanoteignore` uses `FilePattern` syntax (the same library Emanote already uses internally). It is **not** `.gitignore` syntax — there is no negation (`!`), no directory-only marker (`foo/`), and patterns are matched against paths relative to the layer root.

## Layer-scoped semantics

Patterns in a `.emanoteignore` only suppress files inside the layer they live in. If you mount two layers `-L public -L private` and `private/.emanoteignore` lists `secret.md`, a `secret.md` file inside `public/` is **unaffected**. This means individual sub-notebooks can keep private exclusions without leaking those decisions to siblings.

Universal ignores (dotfile directories, editor backup files, the reserved `-/` directory, the `.emanoteignore` file itself) are still enforced globally — those don't need to be repeated per layer.

## Common patterns

| Use case | Pattern |
| --- | --- |
| Obsidian template directory | `templates/**` |
| Daily journal subtree | `journal/**` or `Daily/**` |
| `node_modules/` from a remark setup | `node_modules/**` |
| A single bot/agent doc | `AGENTS.md` |

## When to use the alternative

If you want files visible in your **live server** but excluded from the **published** site, the layer split that `srid` describes in [#228](https://github.com/srid/emanote/issues/228) — `emanote -L ./permanent gen ...` for the public build, `emanote -L .` for the live server — gives you finer control than a single `.emanoteignore` can. `.emanoteignore` is the right tool when you want the file gone from **every** mode.

[filepattern]: https://hackage.haskell.org/package/filepattern
