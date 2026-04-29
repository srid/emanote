---
slug: rawhtml-details
---

# Raw HTML group (issue #433)

CommonMark "type 6" HTML blocks (e.g. `<details>`) end at the next blank
line, so Pandoc emits the open tag, the markdown content, and the close
tag as three separate AST blocks. Without grouping, the markdown
paragraph escapes the surrounding element and ends up as its sibling.
The marker on the inner paragraph is what the e2e step asserts on:
the marker must have a `<details>` ancestor.

<details>

This paragraph **must** render as a child of the `<details>` element.
<span data-marker="RAWHTML_DETAILS_INNER">marker</span>

</details>
