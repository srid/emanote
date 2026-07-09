#!/usr/bin/env python3
"""Generate a synthetic Emanote notebook for memory measurement.

Mimics the shape described in issue #66: ~4500 markdown notes, ~70 MB on disk,
with realistic frontmatter, several wikilinks per note, and a few inline tags.
"""
from __future__ import annotations

import argparse
import os
import random
import string
from pathlib import Path

LOREM = (
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
    "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
    "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris "
    "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in "
    "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
    "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
    "culpa qui officia deserunt mollit anim id est laborum."
)


def rng_word(rng: random.Random) -> str:
    return "".join(rng.choices(string.ascii_lowercase, k=rng.randint(4, 9)))


def note_path(i: int) -> str:
    folder = f"folder{i // 100:02d}"
    return f"{folder}/note{i:04d}.md"


def write_note(root: Path, i: int, total: int, rng: random.Random) -> int:
    relpath = note_path(i)
    fp = root / relpath
    fp.parent.mkdir(parents=True, exist_ok=True)

    # Pick 4-8 wikilink targets at random.
    targets = rng.sample(range(total), k=min(rng.randint(4, 8), total))
    wikilinks = "\n".join(f"- See [[note{j:04d}]]" for j in targets)

    tag_pool = ["work", "personal", "research", "todo", "draft", "idea", "ref"]
    tags = rng.sample(tag_pool, k=rng.randint(1, 3))

    # 20-36 lorem paragraphs ≈ ~15 KiB per note, matching the issue's
    # reported notebook density (4561 files × ~15 KiB).
    body = "\n\n".join(LOREM for _ in range(rng.randint(20, 36)))

    inline_tags = " ".join(f"#{t}" for t in tag_pool[:2])

    content = f"""---
title: "Note number {i}"
tags: {tags!r}
date: 2026-04-{(i % 28) + 1:02d}
---

# Note number {i}

{body}

{inline_tags}

## Related

{wikilinks}

## Notes

Some [external link](https://example.com/) and a `code span`.

> A blockquote with **bold** and *italic*.
"""
    fp.write_text(content)
    return len(content)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("root", type=Path)
    parser.add_argument("--count", type=int, default=4500)
    parser.add_argument("--seed", type=int, default=42)
    args = parser.parse_args()

    args.root.mkdir(parents=True, exist_ok=True)
    rng = random.Random(args.seed)

    # index.md
    (args.root / "index.md").write_text(
        "# Test notebook\n\nUsed for issue-66 memory measurements.\n"
    )

    total_bytes = 0
    for i in range(args.count):
        total_bytes += write_note(args.root, i, args.count, rng)

    print(
        f"Wrote {args.count} notes ({total_bytes / 1024 / 1024:.1f} MiB) under {args.root}"
    )


if __name__ == "__main__":
    main()
